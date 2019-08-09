#ifndef orv_jit_v2_h
#define orv_jit_v2_h

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include <memory>

namespace llvm {
    namespace orc {
	
	class KaleidoscopeJIT {
	private:
	    // Provides context for our current session ie. string
	    // pool, global mutex, etc.
	    ExecutionSession ES;
	    // Used to add object files to our JIT. We wrap it with
	    // compile layer.
	    RTDyldObjectLinkingLayer ObjectLayer;
	    // Used to add LLVM Modules to our JIT.
	    IRCompileLayer CompileLayer;
	    // Transforms and optimizes the LLVM IR emmited.
	    IRTransformLayer TransformLayer;
	    
	    // Symbol mangling.
	    DataLayout DL;
	    MangleAndInterner Mangle;
	    
	    // Will be used to build IR files for the JIT.
	    ThreadSafeContext Ctx;
	    
	public:
	    /**
	     * Instantiates a JIT for the target machine using the
	     * provided target machine builder and data layout.
	     */
	KaleidoscopeJIT(JITTargetMachineBuilder JTMB, DataLayout DL)
		: ObjectLayer(ES,
		    []() { return llvm::make_unique<SectionMemoryManager>(); }),
		CompileLayer(ES, ObjectLayer, ConcurrentIRCompiler(std::move(JTMB))),
		TransformLayer(ES, CompileLayer, optimizeModule),
		DL(std::move(DL)), Mangle(ES, this->DL),
		Ctx(llvm::make_unique<LLVMContext>())
		      {
		ES.getMainJITDylib().setGenerator(
		    cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));
		}
	    
	    /**
	     * Generates a JIT with a data layout and target machine
	     * appropriate for the JIT's host.
	     *
	     * @return a new KaleidoscopeJIT wrapped in llvm's
	     * expected. On error it contains the error that occured
	     * internally on instantiation of the JIT.
	     */
	    static Expected<std::unique_ptr<KaleidoscopeJIT>> Create()
		{
		auto JTMB = JITTargetMachineBuilder::detectHost();
		if ( ! JTMB )
		    return JTMB.takeError();
		
		auto DL = JTMB->getDefaultDataLayoutForTarget();
		if ( ! DL )
		    return DL.takeError();
		
		return llvm::make_unique<KaleidoscopeJIT>(std::move(*JTMB), std::move(*DL));
		}
	    
	    const DataLayout& getDataLayout() const { return DL; }
	    LLVMContext& getContext() { return *Ctx.getContext(); }
	    
	    /** 
	     * Adds a module to the JIT. Does not compile the module
	     * until a lookup is performed on it.
	     */
	    Error addModule(std::unique_ptr<Module> M) {
		return TransformLayer.add(ES.getMainJITDylib(), ThreadSafeModule(std::move(M), Ctx));
		}

	    /**
	     * Looks up a name in the JIT's main dylib. This will
	     * trigger compilation of any modules that contain the
	     * name & have not yet been compiled.
	     */
	    Expected<JITEvaluatedSymbol> lookup(StringRef Name) {
		return ES.lookup({&ES.getMainJITDylib()}, Mangle(Name.str()));
		}
	    
	private:
	    static Expected<ThreadSafeModule>
		optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility& R)
		{
		Module* M = TSM.getModule();
		// Create a function pass manager.
		auto FPM = llvm::make_unique<legacy::FunctionPassManager>(M);
		
		// Add some optimizations.
		FPM->add(createInstructionCombiningPass());
		FPM->add(createReassociatePass());
		FPM->add(createGVNPass());
		FPM->add(createCFGSimplificationPass());
		FPM->doInitialization();
		
		// Run the optimizations over all functions in the module being added to
		// the JIT.
		for (auto &F : *M)
		    FPM->run(F);
		
		return TSM;
		}
	    };
	
	} // namespace orc
    } // namespace llvm

#endif
