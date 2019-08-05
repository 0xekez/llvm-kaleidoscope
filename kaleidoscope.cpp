#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
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
enum Token
  {
    // the end of a file
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_id = -4,
    tok_number = -5,
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
  }
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
    auto proto = llvm::make_unique<Prototype>("", std::vector<std::string>());
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
// std::unique_ptr.
static std::unique_ptr<llvm::Module> TheModule;
// our symbol table. as it stands this wil only hold function params.
static std::map<std::string, Value*> NamedValues;
// our optimizer passer
static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

Value* log_error_v(const char* str) {
  log_error(str);
  return nullptr;
}

/* ===---- optimization ---=== */

void InitializeModuleAndPassManager(void) {
  // open a new module
  TheModule = llvm::make_unique<llvm::Module>("my cool jit", TheContext);

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

Value* CallExpr::codegen() {
  llvm::Function* callee_fn = TheModule->getFunction(callee);
  
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
  // look for an existing `extern` function.
  llvm::Function* the_function = TheModule->getFunction(proto->getName());
  
  if( ! the_function)
    the_function = proto->codegen();
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
      fnir->print(llvm::errs());
      fprintf(stderr, "\n");
      //  InitializeModuleAndPassManager();
    }
  }
  else
    next_tok();
}

static void handle_extern() {
  if (auto pr = parse_extern()) {
    if (auto fnir = pr->codegen()) {
      fprintf(stderr, "Read extern: ");
      fnir->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  }
  else
    next_tok();
}

static void handle_top_level_expression() {
  if (auto fn = parse_top_level_expression()) {
    if (auto fnir = fn->codegen()) {
      //      InitializeModuleAndPassManager();
      fprintf(stderr, "Read top level expression.");
      fnir->print(llvm::errs());
      fprintf(stderr, "\n");
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
  // install standard binary operators.
  // 1 is lowest.
  bop_precedence['<'] = 10;
  bop_precedence['+'] = 20;
  bop_precedence['-'] = 20;
  bop_precedence['*'] = 40;
  // yup nothing else. again, very exotic :)

  fprintf(stderr, "ready> ");
  next_tok(); // prime token

  //  InitializeModuleAndPassManager();
  //  TheModule = llvm::make_unique<llvm::Module>("my cool JIT", TheContext);
  InitializeModuleAndPassManager();

  main_loop();

  TheModule->print(llvm::errs(), nullptr);

  return 0;
}


