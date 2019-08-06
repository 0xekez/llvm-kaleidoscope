// ============
// Kaleidoscope
// ============

// This pretty closely follows LLVM's kaleidoscope tutorial. Other than
// some tiny changes to variable names and namespaces its actually pretty
// much the same. There are also some additional comments that I've
// written as a reference to myself later. You can find the tutorial in
// question at the link below. I'm quite liking it so far.

// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html

// If you'd like to run this code or use it yourself, either go on the
// turorial link and scroll down to where it says how to compile this, or
// run the following command:

// /opt/clang9/bin/clang++ -g -rdynamic kaleidoscope.cpp `/opt/clang9/bin/llvm-config --cxxflags --ldflags --system-libs --libs all` -O3 -o kaleidoscope

// That makes some big assumptions about where your clang9 and llvm
// files are, but you might be able to get the idea from there.

#include "KaleidoscopeJIT.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include <string>
#include <vector>
#include <memory>
#include <map>

using Value = llvm::Value;

/* ===--------------------=== */
/* ===------- lexer ------=== */
/* ===--------------------=== */


// The lexer will return one of these or an ascii value from [0, 255]
// for everything that it sees. current_id_string and current_num_val
// will be populated as appropriate if the current token is an id or
// num.
enum Token{
  // the end of a file
  tok_eof = -1,
  
  // commands
  tok_def = -2,
  tok_extern = -3,
  
  // primary
  tok_id = -4,
  tok_number = -5,
  
  // control flow
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
  tok_for = -9,
  tok_in = -10,
};

// filled in if we are parsing a tok_id
static std::string current_id_string;
// filled in if we are parding a tok_number
static double current_num_val;


/** Get and return the next token from std::in */
static int get_next_tok() {
  static int last_char = ' ';

  // skip over whitespace
  while ( isspace(last_char) )
    last_char = getchar(); // <- c getchar function

  // look for ids and keywords
  if ( isalpha(last_char) ) {
    current_id_string = last_char;
    // can contain, but can't start with numbers
    while ( isalnum( (last_char = getchar()) ) )
      current_id_string += last_char;
    
    if ( current_id_string == "def" )
      return tok_def;
    if ( current_id_string == "extern" )
      return tok_extern;
    if ( current_id_string == "if" )
      return tok_if;
    if ( current_id_string == "then" )
      return tok_then;
    if ( current_id_string == "else" )
      return tok_else;
    if ( current_id_string == "for" )
      return tok_for;
    if ( current_id_string == "in" )
      return tok_in;
    return tok_id;
  }

  // numbers. .7 is legal grahmar.
  // won't read 12.3.45 propperly
  else if ( isdigit(last_char) || last_char == '.' ) {
    std::string num;
    do {
      num += last_char;
      last_char = getchar();
    } while ( isdigit(last_char) || last_char == '.' );

    current_num_val = strtod(num.c_str(), 0);
    return tok_number;
  }

  // comments
  if ( last_char == '#' ) {
    do {
      last_char = getchar();
    } while ( last_char != EOF && last_char != '\n' && last_char != '\r');

    if (last_char != EOF)
      return get_next_tok();
  }

  // now we either have an operator character like + or EOF
  if ( last_char == EOF )
    return tok_eof;

  // we don't know anything about it, so we'll just return the ascii value
  int this_char = last_char;
  last_char = getchar();
  return this_char;
}

/* ===--------------------=== */
/* ===-------- ast -------=== */
/* ===--------------------=== */

// TOOD(zeke): we should use the visitor pattern for these.

// base class for expression nodes
class Expr {
public:
  virtual ~Expr() {}
  virtual Value *codegen() = 0;
};
// exotic, I know

class NumberExpr : public Expr {
private:
  double val;
public:
  NumberExpr(double val): val(val) {}
  Value *codegen() override;
};

class VarExpr : public Expr {
private:
  std::string name;
public:
  // TODO(zeke): take ownership?
  VarExpr(const std::string& name): name(name) {}
  Value *codegen() override;
};

class BinaryExpr : public Expr {
private:
  char op; // the operation being applied
  std::unique_ptr<Expr> left, right;
public:
  BinaryExpr(char op, std::unique_ptr<Expr> left,
	     std::unique_ptr<Expr> right):
    op(op), left(std::move(left)), right(std::move(right)) {}
  Value *codegen() override;
};

class IfExpr : public Expr {
private:
  std::unique_ptr<Expr> _cond, _then, _else;
public:
  IfExpr(std::unique_ptr<Expr> _cond,
	 std::unique_ptr<Expr> _then,
	 std::unique_ptr<Expr> _else)
    : _cond(std::move(_cond)), _then(std::move(_then)), _else(std::move(_else)) {}
  Value* codegen() override;
};

class ForExpr : public Expr {
private:
  std::string var_name;
  std::unique_ptr<Expr> start, end, step, body;
public:
  ForExpr(const std::string& var_name, std::unique_ptr<Expr> start,
	  std::unique_ptr<Expr> end, std::unique_ptr<Expr> step,
	  std::unique_ptr<Expr> body)
    : var_name(var_name), start(std::move(start)), end(std::move(end)),
      step(std::move(step)), body(std::move(body)) {}

  Value* codegen() override;
  std::string getVarName() { return var_name; }
};

class CallExpr : public Expr {
private:
  std::string callee;
  std::vector<std::unique_ptr<Expr>> args;
public:
  CallExpr(const std::string& callee,
	   std::vector<std::unique_ptr<Expr>> args):
    callee(callee), args(std::move(args)) {}
  Value *codegen() override;
};

// the "prototype" for a function. captures its name, argument names,
// and number of arguments (implicitly with argument names)
class Prototype {
private:
  std::string name;
  std::vector<std::string> args;
public:
  Prototype(const std::string& name, std::vector<std::string> args):
    name(name), args(args) {}
  llvm::Function* codegen();
  std::string getName() { return name; }
};

// the actual function definition. functions own their prototypes.
class Function {
private:
  std::unique_ptr<Prototype> proto;
  std::unique_ptr<Expr> body;
public:
  Function(std::unique_ptr<Prototype> proto,
	   std::unique_ptr<Expr> body):
    proto(std::move(proto)), body(std::move(body)) {}
  llvm::Function* codegen();
};

/* ===--------------------=== */
/* ===------ parser ------=== */
/* ===--------------------=== */

// simple token buffer. current_tok is the token the parser is looking
// at, next_tok gets the next token, stores it in current_tok and yields
// that token. this allows for simple lookahead.
static int current_tok;
static int next_tok() {
  return current_tok = get_next_tok();
}

static std::unique_ptr<Expr> parse_expression();
static std::unique_ptr<Expr> parse_if();

// Very, very, very simple error handling.
std::unique_ptr<Expr> log_error(const char* str) {
  fprintf(stderr, "LogError: %s\n", str);
  return nullptr;
}
std::unique_ptr<Prototype> log_error_p(const char* str) {
  log_error(str);
  return nullptr;
}

// NumberExpr -> number
static std::unique_ptr<Expr> parse_number() {
  auto result = llvm::make_unique<NumberExpr>(current_num_val);
  // consume the number
  next_tok();
  return std::move(result);
}

// ParenExpr -> '(' expression ')'
static std::unique_ptr<Expr> parse_parens() {
  // eat '('
  next_tok();
  auto v = parse_expression();
  if ( ! v )
    return nullptr;

  if (current_tok != ')')
    return log_error("expected a closing ')'");

  // eat ')'
  next_tok();
  return v;
}

// for <start>, <end>, <step> in <body>
// the step value is optional.
static std::unique_ptr<Expr> parse_for() {
  next_tok(); // eat for
  if (current_tok != tok_id)
    return log_error("Expected id after 'for'");
  std::string start_name = current_id_string;
  next_tok(); // eat id

  if (current_tok != '=')
    return log_error(std::string("Expected '=' after '" + start_name + "' in for.").c_str());
  next_tok(); // eat '='

  auto start = parse_expression();
  if (! start ) return nullptr;
  if ( current_tok != ',' )
    return log_error("Expected ',' after start (assignment) clause of for loop");
  next_tok();

  auto end = parse_expression();
  if ( ! end ) return nullptr;

  std::unique_ptr<Expr> step;
  // optional step
  if ( current_tok == ',' ){
    next_tok();
    step = parse_expression();
    if ( ! step ) return nullptr;
  }

  if (current_tok != tok_in)
    return log_error("Expected 'in' after for.");
  next_tok();

  auto body = parse_expression();
  if ( ! body ) return nullptr;

  return llvm::make_unique<ForExpr>(start_name, std::move(start), std::move(end),
				    std::move(step), std::move(body));
}

// identifier ->
//     | identifier
//     | identifier '(' expression* ')'
static std::unique_ptr<Expr> parse_id() {
  std::string name(std::move(current_id_string));
  next_tok(); // eat id

  if (current_tok != '(')
    return llvm::make_unique<VarExpr>(std::move(name));
  
  // a call!
  next_tok(); // eat '('
  std::vector<std::unique_ptr<Expr>> args;

  if (current_tok != ')')
    while (true) { // we manually break
      if ( auto arg = parse_expression() )
	args.push_back(std::move(arg));
      else
	return log_error("bad function argument");

      if (current_tok == ')')
	break;
      if (current_tok != ',')
	return log_error("expected ',' or ')' at position");
      next_tok(); // eat ','
    }

  next_tok(); // eat ')'

  return llvm::make_unique<CallExpr>(std::move(name), std::move(args));
}

// primary ->
//    | identifier
//    | number
//    | paren
static std::unique_ptr<Expr> parse_primary() {
  switch (current_tok){
  default:
    return log_error("unknown token when expecting an expression");
  case tok_id:
    return parse_id();
  case tok_number:
    return parse_number();
  case '(':
    return parse_parens();
  case tok_if:
    return parse_if();
  case tok_for:
    return parse_for();
  }
}

// if expr then expr else expr;
static std::unique_ptr<Expr> parse_if() {
  next_tok(); // eat if
  
  auto condition = parse_expression();
  if (! condition)
    return nullptr;
  
  if (current_tok != tok_then)
    return log_error("expected 'then` after if expression");
  next_tok(); // eat then
  auto then = parse_expression();
  if ( ! then ) return nullptr;
  
  if (current_tok != tok_else)
    return log_error("expected `else` after then expression");
  next_tok(); // eat else
  auto _else = parse_expression();
  if ( ! _else ) return nullptr;

  return llvm::make_unique<IfExpr>(std::move(condition), std::move(then),
				   std::move(_else));
}

/* ===---- binary ops ----=== */

// holds the precedence for each defined binary operator
static std::map<char, int> bop_precedence;

// get the precedence of the current token
static int get_tok_precedence() {
  if ( ! isascii(current_tok) )
    return -1;
  
  int tok_p = bop_precedence[current_tok];
  if (tok_p <= 0) return -1;
  return tok_p;
}

// bin-op-right ->
//    | ('+' primary)*
// @param prec the minimum precidence operator that this function
//  is allowed to eat.
// @param left the expression that we are building
static std::unique_ptr<Expr> parse_b_op_right(int prec,
					      std::unique_ptr<Expr> left) {
  while (true) {
    int tok_prec = get_tok_precedence();
    
    // if we've met a token we are not allowed to eat
    if (tok_prec < prec)
      return left;
    
    // ok, we know its a legal one for us to eat
    int bin_op = current_tok;
    next_tok(); // eat the operator

    auto right = parse_primary();
    if ( ! right )
      return nullptr;

    // who is more tightly bonded with right?
    // ie. who should use?
    int next_prec = get_tok_precedence();
    if (tok_prec < next_prec) {
      // right gets it, so we merge it in and parse.
      right = parse_b_op_right(tok_prec + 1, std::move(right));
      if ( ! right )
	return nullptr;
    }
    // we get it.
    left = llvm::make_unique<BinaryExpr>(bin_op, std::move(left),
					std::move(right));
  } // while
}

// expression ->
//    | primary b-op-right
static std::unique_ptr<Expr> parse_expression() {
  auto left = parse_primary();
  if (! left )
    return nullptr;
  return parse_b_op_right(0, std::move(left));
}


/* ===---- functions ----=== */

// prototype ->
//    | id '(' id* ')'
static std::unique_ptr<Prototype> parse_prototype() {
  if (current_tok != tok_id)
    return log_error_p("expected function name in prototype");

  std::string fn_name(std::move(current_id_string));
  next_tok(); // consume the id

  if (current_tok != '(')
    return log_error_p("expected '(' after function name");

  std::vector<std::string> arg_names;
  while ( next_tok() == tok_id )
    arg_names.push_back(std::move(current_id_string));
  if (current_tok != ')')
    return log_error_p("expected closing ')' in prototype");
  
  next_tok(); // eat ')'

  return llvm::make_unique<Prototype>(std::move(fn_name), std::move(arg_names));
}

// definition -> 'def' prototype expression
static std::unique_ptr<Function> parse_definition(){
  next_tok(); // eat 'def'
  auto proto = parse_prototype();
  if ( ! proto ) return nullptr;

  if ( auto e = parse_expression() )
    return llvm::make_unique<Function>(std::move(proto), std::move(e));

  return nullptr;
}

// external -> 'extern' prototype
static std::unique_ptr<Prototype> parse_extern() {
  next_tok(); // eat 'extern'
  return parse_prototype();
}

// toplevelexpression -> expression
static std::unique_ptr<Function> parse_top_level_expression() {
  if (auto e = parse_expression()) {
    auto proto = llvm::make_unique<Prototype>("__anon_expr", std::vector<std::string>());
    return llvm::make_unique<Function>(std::move(proto), std::move(e));
  }
  return nullptr;
}

/* ===---------------------=== */
/* ===-------- llvm -------=== */
/* ===---------------------=== */

// holds many core datastrcutres and tables. no need to understand.
static llvm::LLVMContext TheContext;
// helper for creating llvm instructions.
static llvm::IRBuilder<> Builder(TheContext);
// owns memory for values and functions and global variables. its 
// handling of memory means that Value is a naked ptr and not a
static std::unique_ptr<llvm::Module> TheModule;
// our symbol table. as it stands this wil only hold function params.
static std::map<std::string, Value*> NamedValues;
// our optimizer passer
static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;
// the JIT compiler
static std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
// maps names to their most recent function declarations
static std::map<std::string, std::unique_ptr<Prototype>> FunctionProtos;

Value* log_error_v(const char* str) {
  log_error(str);
  return nullptr;
}

/* ===---- optimization ---=== */

void InitializeModuleAndPassManager(void) {
  // open a new module
  TheModule = llvm::make_unique<llvm::Module>("my cool jit", TheContext);
  TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

  // associate a pass manager
  TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->add(llvm::createCFGSimplificationPass());

  TheFPM->doInitialization();
}

/* ===------ code gen -----=== */

// numbers are represented with the ConstantFP class.
Value* NumberExpr::codegen() {
  return llvm::ConstantFP::get(TheContext, llvm::APFloat(val));
}

Value* VarExpr::codegen() {
  Value* v = NamedValues[name];
  if ( ! v )
    return log_error_v("unknown variable name");
  return v;
}

// specifics here: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html#expression-code-generation
Value* BinaryExpr::codegen() {
  Value* l = left->codegen();
  Value* r = right->codegen();
  if ( ! (l && r) )
    return nullptr;
  
  switch (op) {
  case '+':
    return Builder.CreateFAdd(l, r, "addtmp");
  case '-':
    return Builder.CreateFSub(l, r, "subtmp");
  case '*':
    return Builder.CreateFMul(l, r, "multmp");
  case '<':
    l = Builder.CreateFCmpULT(l, r, "cmptmp");
    // convert bool to double. kaleidoscope only has a double type.
    return Builder.CreateUIToFP(l, llvm::Type::getDoubleTy(TheContext),
				"booltmp");
  default:
    return log_error_v("invalid binary operator");
  }
}

Value* ForExpr::codegen() {
  Value* start_val = start->codegen();
  if ( ! start_val ) return nullptr;
  
  // The body
  llvm::Function* the_function = Builder.GetInsertBlock()->getParent();
  llvm::BasicBlock* PreBB = Builder.GetInsertBlock();
  llvm::BasicBlock* BodyBB = llvm::BasicBlock::Create(TheContext, "loop", the_function);

  // Make a fallthrough from the current block to the loop block
  Builder.CreateBr(BodyBB);

  // Fill up the body
  Builder.SetInsertPoint(BodyBB);
  llvm::PHINode* variable = Builder.CreatePHI(llvm::Type::getDoubleTy(TheContext),
					      2, var_name.c_str());

  variable->addIncoming(start_val, PreBB);
  // Need to redefine anything we shadow so we save it now
  Value* old_val = NamedValues[var_name];
  NamedValues[var_name] = variable;

  if ( ! body->codegen() )
    return nullptr;

  Value* step_val = nullptr;
  if (step) {
    step_val = step->codegen();
    if ( ! step_val ) return nullptr;
  } else {
    // default to 1
    step_val = llvm::ConstantFP::get(TheContext, llvm::APFloat(1.0));
  }
  // add the step to the loop variable
  Value* NextVar = Builder.CreateFAdd(variable, step_val, "nextvar");

  Value* end_cond = end->codegen();
  if ( ! end_cond ) return nullptr;

  // convert to a bool. as before, only 0.0 is false.
  end_cond = Builder.CreateFCmpONE(end_cond, llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), "loopcond");

  // should the loop exit?
  llvm::BasicBlock* LoopEndBB = Builder.GetInsertBlock();
  llvm::BasicBlock* AfterBB = llvm::BasicBlock::Create(TheContext, "afterloop", the_function);

  // insert conditional
  Builder.CreateCondBr(end_cond, BodyBB, AfterBB);

  // code after the loop goes into afterbb
  Builder.SetInsertPoint(AfterBB);

  // cleanup
  variable->addIncoming(NextVar, LoopEndBB);

  // restore the shadowed variables
  if (old_val)
    NamedValues[var_name] = old_val;
  else
    NamedValues.erase(var_name);

  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(TheContext));
}

Value* IfExpr::codegen() {
  Value* cond_val = _cond->codegen();
  if ( ! cond_val ) return nullptr;

  // The only thing that is false is 0.0.
  // convert cond_val to a bool by comparing /= false.
  cond_val = Builder.CreateFCmpONE(cond_val, 
				   llvm::ConstantFP::get(TheContext, llvm::APFloat(0.0)), 
				   "ifcond");
  // build the blocks for our if statement
  llvm::Function* the_function = Builder.GetInsertBlock()->getParent();

  llvm::BasicBlock* ThenBB =
    llvm::BasicBlock::Create(TheContext, "then", the_function);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(TheContext, "ifcont");

  Builder.CreateCondBr(cond_val, ThenBB, ElseBB);

  // Emit then.
  Builder.SetInsertPoint(ThenBB);
  Value* then_val = _then->codegen();
  if ( ! then_val ) return nullptr;

  Builder.CreateBr(MergeBB);
  // later we'd like to go back and keep executing from the end point of the 
  // then block, we go ahead an set the then block to where all of the code wrapped
  // up here. It might seem silly, but the reason we have to update it is because
  // everything is an expression and the current insert block could have moved
  // if there were more if expressions in the condition.
  ThenBB = Builder.GetInsertBlock();

  // Emit else.
  // now that the then has been attached we can attach the else.
  the_function->getBasicBlockList().push_back(ElseBB);
  Builder.SetInsertPoint(ElseBB);

  Value* else_val = _else->codegen();
  if ( ! else_val ) return nullptr;

  Builder.CreateBr(MergeBB);
  // same as above long comment, possible the insert block has changed during
  // the code generation.
  ElseBB = Builder.GetInsertBlock();

  // Emit merge block
  the_function->getBasicBlockList().push_back(MergeBB);
  Builder.SetInsertPoint(MergeBB);
  
  // a phi-node is something that before this I did not know existed.
  // basically, it remembers where the flow is coming from and uses the value
  // that comes from that section of the code. This makes it possible to
  // deal with emitting LLVM IR without doing a bunch of complicated stuff
  // that we will have to do later anyway :)
  llvm::PHINode* PN = Builder.CreatePHI(llvm::Type::getDoubleTy(TheContext), 2, "iftmp");

  PN->addIncoming(then_val, ThenBB);
  PN->addIncoming(else_val, ElseBB);
  return PN;
}

llvm::Function* getFunction(std::string name) {
  // Is it in the current module?
  if (auto* F = TheModule->getFunction(name))
    return F;

  // Does it exist somewhere else?
  auto where = FunctionProtos.find(name);
  if (where != FunctionProtos.end())
    return where->second->codegen();

  return nullptr;
}

Value* CallExpr::codegen() {
  llvm::Function* callee_fn = getFunction(callee);
  
  if ( ! callee_fn )
    return log_error_v("unknown function reference");
  
  if ( callee_fn->arg_size() != args.size() )
    return log_error_v("incorrect number of arguments passed to function.");
  
  std::vector<Value*> args_v;
  
  for (const auto& expr : args)
    if ( auto code = expr->codegen() )
      args_v.push_back(code);
    else
      return nullptr;
  
  return Builder.CreateCall(callee_fn, args_v, "calltmp");
}


llvm::Function* Prototype::codegen() {
  // Create a vector of N doubles.
  std::vector<llvm::Type*> doubles(args.size(),
				   llvm::Type::getDoubleTy(TheContext));
  // Create a function type that takes N doubles and returns a double.
  llvm::FunctionType* FT =
    llvm::FunctionType::get(llvm::Type::getDoubleTy(TheContext), doubles, false);
  
  // Create the actual function and install it in TheModule's symbol table.
  llvm::Function* F =
    llvm::Function::Create(FT, llvm::Function::ExternalLinkage, name, TheModule.get());

  // Install all of the argument names.
  unsigned idx = 0;
  for(auto& Arg : F->args())
    Arg.setName(args[idx++]);

  // At this point, we have a function with no body. For extern functions this is fine,
  // and for ones that we define we'll 
  return F;
}

llvm::Function* Function::codegen() {
  // Get its name.
  std::string name = proto->getName();
  // Transfer ownership.
  FunctionProtos[proto->getName()] = std::move(proto);
  // look it up.
  llvm::Function* the_function = getFunction(name);
  
  if ( ! the_function)
    return nullptr; // we failed.
  if ( ! the_function->empty() )
    // :(
    return static_cast<llvm::Function*>(log_error_v("Functions can not be redefined."));

  // Create a new block to start putting code in.
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", the_function);
  Builder.SetInsertPoint(BB);

  NamedValues.clear();
  for(auto& Arg : the_function->args())
    NamedValues[Arg.getName()] = &Arg;

  if ( Value* RetVal = body->codegen()) {
    Builder.CreateRet(RetVal);
    // Validate the generated code with some llvm magic.
    llvm::verifyFunction(*the_function);

    TheFPM->run(*the_function);

    return the_function;
  }

  // Something went wrong - cleanup
  the_function->eraseFromParent();
  return nullptr;
}


/* ===--------------------=== */
/* ===------- Main -------=== */
/* ===--------------------=== */

static void handle_definition() {
  if (auto fn = parse_definition()) {
    if (auto fnir = fn->codegen()) {
      fprintf(stderr, "Read function definition: ");
      // fnir->print(llvm::errs());
      fprintf(stderr, "\n");
      TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();
    }
  }
  else
    next_tok();
}

static void handle_extern() {
  if (auto pr = parse_extern()) {
    if (auto fnir = pr->codegen()) {
      fprintf(stderr, "Read extern: ");
      // fnir->print(llvm::errs());
      fprintf(stderr, "\n");
      FunctionProtos[pr->getName()] = std::move(pr);
    }
  }
  else
    next_tok();
}

static void handle_top_level_expression() {
  if (auto fn = parse_top_level_expression()) {
    if (fn->codegen()) {
      // JIT the module containing the anonymous expression, keep a
      // handle so that we can free later.
      auto H = TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
      assert(ExprSymbol && "Function not found");

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = (double (*)())(intptr_t)(ExprSymbol.getAddress().get());
      fprintf(stderr, "Evaluated to %f\n", FP());

      TheJIT->removeModule(H);
    }
  }
  else
    next_tok();
}

static void main_loop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch(current_tok) {
    default:
      handle_top_level_expression();
      break;
    case tok_eof:
      return;
    case ';':
      next_tok();
      break;
    case tok_def:
      handle_definition();
      break;
    case tok_extern:
      handle_extern();
      break;
    }
  }
}

int main() {
  // Set up llvm for our machine
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // install standard binary operators.
  // 1 is lowest.
  bop_precedence['<'] = 10;
  bop_precedence['+'] = 20;
  bop_precedence['-'] = 20;
  bop_precedence['*'] = 40;
  // yup nothing else. again, very exotic :)

  fprintf(stderr, "ready> ");
  next_tok(); // prime token

  TheJIT = llvm::make_unique<llvm::orc::KaleidoscopeJIT>();
  InitializeModuleAndPassManager();

  main_loop();

  TheModule->print(llvm::errs(), nullptr);

  return 0;
}
