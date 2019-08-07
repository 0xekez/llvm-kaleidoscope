/**
 * This is a simple JIT compiler following along with LLVM's tutorial:
 *
 *     https://releases.llvm.org/6.0.0/docs/tutorial/BuildingAJIT1.html 
 *
 * It uses ORCv1 though and as a result is not compatable with the
 * newer ORCv2 JIT API. I left some comments above the methods to
 * cover roughly how it works, but this is pretty much dead in the
 * water because we want to target ORCv2.
 */

#ifndef my_jit_h
#define my_jit_h

#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

#include <memory>
#include <string>

namespace llvm {
  namespace orc {

    class KaleidoscopeJIT {
    private:
      // The machine we'll build our compiler on.
      std::unique_ptr<TargetMachine> TM;
      // Used for symbol mangling.
      const DataLayout DL;
      // ORC __layers__
      RTDyldObjectLinkingLayer ObjectLayer;
      IRCompileLayer<decltype(ObjectLayer), SimpleCompiler> CompileLayer;

    public:
      // Handle that will be returned from our AddModule method.
      using ModuleHandle = decltype(CompileLayer)::ModuleHandleT;

      /**
       * Constructs an instance of the JIT.
       * 
       * Initializes a target machine and data layout for the curent
       * process. Uses LLVM's off the shelf memory manager and
       * compiler instance. Once initialization is complete, 'loads'
       * the host process and makes its symbols avaliable for execution.
       */
      KaleidoscopeJIT()
	: TM(EngineBuilder().selectTarget()), DL(TM->createDataLayout()),
	  ObjectLayer([]() { return std::make_shared<SectionMemoryManager>(); }),
	  CompileLayer(ObjectLayer, SimpleCompiler(*TM)) {
	// Passing a nullptr instead of a path to a dynamic library 
	// causes this to load host process.
	llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
      }

      /**
       * Adds IR to the JIT and makes it available for execution.
       *
       * To add a module to the compile layer we need both the module
       * and a symbol resolver for that module. The resolver consists
       * of two functions. The first one looks back into the JIT
       * itself for symbols that are part of the same logical
       * dylib. The lambda is called if resolution in the first one
       * fails and looks for symbols with definitions in other modules
       * that have already been added to the JIT. 
       */
      ModuleHandler addModule(std::unique_ptr<Module> M) {
	auto Resolver = createLambdaResolver(
	  [&](const std::string& name) {
	    if (auto Smy = CompileLayer.findSymbol(Name, false))
	      return Sym;
	    return JITSymbol(nullptr);
	  },
	  [](const std::string& Name) {
	    if (auto SymAddr = 
		RTDyldMemoryManager::getSymbolAddressInProcess(Name))
	      return JITSymbol(SymAddr, JITSymbolFlags::Exported);
	    return JITSymbol(nullptr);
	  });

	return cantFail(CompileLayer.addModule(std::move(M),
					       std::move(Resolver)));
      }
      
      /**
       * Finds a symbol that has been added to our JIT.
       *
       * To make itself more compatable with compiled code ORC JIT
       * uses mangled symbols so in order to perform the lookup, we
       * mangle the string and then search for that.
       */
      JITSymbol findSymbol(const std::string Name) {
	std::string MangledName;
	raw_string_ostream MangledNameStream(MangledName);
	Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
	return CompileLayer.findSymbol(MangledNameStream.str(), true);
      }

      JITTargetAddress getSymbolAddress(const std::string Name) {
	return cantFail(findSymbol(Name).getAddress());
      }
    }; // JIT

  } // namespace orc
} // namespace llvm

#endif
