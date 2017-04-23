
module TransInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, analyze, NumVal, ClosureVal



#OWLlllllllllllllllllllllllllllllllllllllllllllllllllll

abstract OWL

type NumNode <: OWL
  n::Real
end

type Binop <: OWL
  op::Function
  lhs::OWL
  rhs::OWL
end

type NewAdd <: OWL #02/23/17
  op::Function
  operands::Array{OWL}
end

type Unop <: OWL
  op::Function
  operand::OWL
end


type If0Node <: OWL
  condition::OWL
  zero_branch::OWL
  nonzero_branch::OWL
end

#02/20/17
type Bind <: OWL
    variable::Symbol
    owl::OWL
end

type WithNode <: OWL
  #name::Symbol #::Array{Symbol} old stuff
  binding_expr::Array{Bind}  #num or clasure val in calc time. Error it not match what need OWL
  body::OWL
end

type IdNode <: OWL
  name::Symbol
end

type FuncDefNode <: OWL
  formal_parameter::Array{Symbol} #::Symbol old stuff THESE TWO LINKED IN A CALC
  fun_body::OWL
end

type FuncAppNode <: OWL
  fun_expr::OWL
  arg_expr::Array{OWL} #OWL old stuff
end

type AndNode <: OWL
  opArray::Array{OWL}
end

# RetValllllllllllllll and environmenttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
abstract RetVal
abstract Environment

type NumVal <: RetVal
  n::Real
end

type ClosureVal <: RetVal
  param::Array{Symbol}
  body::OWL
  env::Environment # this is the environment at definition time!
end

type mtEnv <: Environment
end

type ConcreteEnvironment <: Environment
  name::Symbol
  value::RetVal
  parent::Environment
end



function collatz(retval :: NumVal)
    num = collatz_helper( retval.n, 0 )
   return NumVal(num)
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

op_table = Dict(
  :+ => +,
  :- => -,
  :* => *,
  :/ => /,
  :mod => mod,
  :collatz => collatz
)

# parseeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee


function parse( expr::Any )
    throw(LispError("Not just any expression will work here!"))
end

function parse( expr::Real )
    #println("parse::Real")
  return NumNode( expr )
end

function parse( expr::Symbol )
    #println("parse::Symbol")
    res = validReserved( expr )
    if(res == 0)
        throw(LispError("Invalid Symbol!"))
    end
    return IdNode( expr )
end

function parse( expr::Array{Any} )
  # should hand us an array, should be length 3
  #println("parse::Array{Any}")
  #println(expr)

  #looking for RunInt stuff
  if ((get(op_table, expr[1], 0) == +) || (get(op_table, expr[1], 0) == -) ||
        (get(op_table, expr[1], 0) == *) || (get(op_table, expr[1], 0) == /) ||
        (get(op_table, expr[1], 0) == mod)) && (length(expr) == 3)

    try
      #println("Binop")
      #println( expr)
      #println(get(op_table, expr[1], 0))
      if( length(expr) == 3)
        return Binop( get(op_table, expr[1], 0), parse(expr[2]), parse(expr[3]) )
      end
    catch
      #println(LOAD_PATH)
      throw(LispError("Something went wrong with 3 expressions!"))
    end
  elseif ((get(op_table, expr[1], 0) == +) && (length(expr) > 3))
    #println("newAdd in parse")
    try
      newrray = OWL[]
      for j=2:length(expr)
        #println("j is " * string(j))
        #println(expr[j])
        push!(newrray, parse(expr[j]))
      end
      return NewAdd( get(op_table, expr[1], 0), newrray )
    catch
      throw(LispError("Something went wrong with NewAdd!"))
    end
  elseif (length(expr) == 2) &&  ((get(op_table, expr[1], 0) == -) || (get(op_table, expr[1], 0) == collatz))
    try
        #println( expr[1], expr[2])
        #println(get(op_table, expr[1], 0))
        return Unop( get(op_table, expr[1], 0), parse(expr[2]))
    catch
        #println(LOAD_PATH)
        throw(LispError("Something went wrong with 2 expressions!"))
    end
  #error checks
  elseif (length(expr) == 3) && (get(op_table, expr[1], 0) == collatz)
    throw(LispError("Too many arguments for collatz!"))
  elseif (length(expr) == 2) && ((get(op_table, expr, 0) == +) || (get(op_table, expr, 0) == -) ||
        (get(op_table, expr, 0) == *) || (get(op_table, expr, 0) == /) || (get(op_table, expr, 0) == mod))
    throw(LispError("Too many arguments for +!"))

  #new stufffffffffffffffffffffffffffffffffffffffffffffffffffffffff
  elseif expr[1] == :if0#parms check
    if (length(expr) != 4)
      throw(LispError("Something went wrong with if0! Not right length!"))
    end
    try
      return If0Node( parse(expr[2]), parse(expr[3]), parse(expr[4]) )
    catch
      #println(LOAD_PATH)
      throw(LispError("Something went wrong with if0"))
    end


  #WithNodes will be generated into mulit-arg lambda nodes
  elseif expr[1] == :with
    #println("in with Parse")
    #println(expr)
    if (length(expr) != 3)
      throw(LispError("Something went wrong with With! Not right length!"))
    end
    #rec func
    withHelpResult = 0
    try
      withHelpResult = withHelp(expr[2])
    catch
      throw(LispError("Something went wrong with WithHelp function!"))
    end
    #println("withHelpResult:")
    #println(withHelpResult)
    if(withHelpResult == 0)
      throw(LispError("Something went wrong with With! WithHelp Failed!"))
    end

    toWith = bindBuilder(expr[2])
    try
      return WithNode( toWith, parse(expr[3]) )
    catch
      #println(LOAD_PATH)
      throw(LispError("Something went wrong with With!"))
    end


  elseif expr[1] == :lambda
    #println("lambda here")
    if (length(expr) != 3)
      throw(LispError("Something went wrong with lambda/FDN! Not right length!"))
    end
    try
      #println("createFuncDefNode")#multiple params
      #println(expr)

      return FuncDefNode( expr[2] , parse(expr[3]) )
    catch
      #println(LOAD_PATH)
      throw(LispError("Something went wrong with lambda/FDN!"))
    end

  #(and <OWL> <OWL> <OWL>*)  # new operation 'and' ONLY IF0 nodes
  elseif expr[1] == :and
    #println("parse: and")
    if (length(expr) < 3)
      throw(LispError("Something went wrong with add! Not right length!"))
    end

    try
      #println("newAnd")
      #println(expr)
      newrray = OWL[]
      for j=2:length(expr)
        #println("j is " * string(j))
        #println(expr[j])
        push!(newrray, parse(expr[j]))
      end
      return AndNode(newrray )
    catch
      #println(LOAD_PATH)
      throw(LispError("Something went wrong with and!"))
    end

  else
    #println("funcAppNode")
    #println(expr)

    newrray = OWL[]
    for j=2:length(expr)
      #println("j is " * string(j))
      #println(expr[j])
      push!(newrray, parse(expr[j]))
    end

    return FuncAppNode( parse(expr[1]), newrray )
  end
end

#helper functionsssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
function bindBuilder( expr::Array{Any} )
  #println("bindBuilder")

  returnVal = Any[]
  for j=1:length(expr)
    row = expr[j]
    #println(row)
    newBind = Bind(row[1], parse(row[2]))
    #println("gonna append3")
    push!(returnVal, newBind)
  end
  #println("returnVal:")
  #println(returnVal)
  return returnVal
end

function withHelp( expr::Array{Any} )
  #println("withHelp")
  for j=1:length(expr)
    #println(expr[j])
    #println("before length")
    #println(length(expr[j]))
    if(length(expr[j]) != 2)
      #println("fail in withHelp")
      return 0
    end
    row = expr[j]
    num = validReserved(row[1])
    if(num == 0)
      #println("fail in withHelp not Valid")
      return 0
    end
  end
  return 1
end

function validReserved( expr::Symbol )
    #println("validReserved:")
    #println(expr)
    if (get(op_table, expr, 0) == +) || (get(op_table, expr, 0) == -) ||
        (get(op_table, expr, 0) == *) || (get(op_table, expr, 0) == /) ||
        (get(op_table, expr, 0) == mod) || (get(op_table, expr, 0) == collatz) ||
        (expr == :if0) || (expr == :with) || (expr == :lambda)

        #println("It's a reseved symbol")
        return 0
    end
    #println("It's NOT a reseved symbol")
    return 1
end

#analyzeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

function analyze(ast::OWL)
  throw( LispError( "Analyze - Unknown node!" ) )
end

function analyze(ast::NumNode)
  return ast
end

function analyze(ast::IdNode)
  return ast
end

function analyze(ast::Binop)
  #println("analyze::Binop")
  #println(ast)
  return Binop(ast.op, analyze(ast.lhs), analyze(ast.rhs))
end

function analyze(ast::Unop)
  #println("analyze::Unop")
  #println(ast)
  return Unop(ast.op, analyze(ast.operand))
end

function analyze(ast::NewAdd)# return a big Binop string
  #println("analyze::NewAdd")
  #println(ast)
  numss = ast.operands
  #backwards make a Binop
  #println("here we go!")
  nums = reverse(numss)
  #println(nums)
  prevThing = "no"
  newBinop = 0
  for j=1:length(nums)
    thing = analyze(nums[j])
    #println(thing)
    #println("prevThing:")
    #println(prevThing)
    if(prevThing != "no")
      newBinop = Binop(ast.op, nums[j], prevThing)
      #println(newBinop)
      prevThing = newBinop
    else
      prevThing = nums[j]
    end
  end

  #println(newBinop)
  return newBinop
end

function analyze(ast::WithNode)#turn into a lambda
  #println("analyze::WithNode")
  #println(ast)

  #form params x y
  newFormalParams = Symbol[]
  #actual params 1 2
  newParams = OWL[]

  for j=1:length(ast.binding_expr)
    thing = ast.binding_expr[j]
    #println(thing)
    push!(newFormalParams, thing.variable)
    push!(newParams, analyze(thing.owl))
  end

  closure_body = analyze( ast.body )
  fdn = FuncDefNode( newFormalParams, closure_body )
  #actual_parameter_expr = analyze( ast.binding_expr )
  return FuncAppNode( fdn, newParams )
end

function analyze(ast::AndNode)
  #println("analyze::AndNode")
  #println(ast)
  newIf0 = 0

  expressionss = ast.opArray
  #backwards make a If0Node
  #println("here we go!")
  expressions = reverse(expressionss)
  #println(expressions)
  newIf0 = If0Node(expressions[1], NumNode(0), NumNode(1))
  prevThing = newIf0
  for j=2:length(expressions)#1 or more left in array
    thing = analyze(expressions[j])
    #println(thing)
    #println("prevThing:")
    #println(prevThing)
    newIf0 = If0Node( expressions[j], NumNode(0), prevThing)
    #println(newIf0)
    prevThing = newIf0
  end
  #println("prevThing:")
  #println(prevThing)
  return prevThing
end

function analyze(ast::FuncDefNode)
  #println("analyze::FuncDefNode")
  #println(ast)
  return FuncDefNode( ast.formal_parameter, analyze( ast.fun_body) )#multiple formal_parmeter is the symbols.
end

function analyze(ast::FuncAppNode)
  #println("analyze::FuncAppNode")
  #println(ast)
  newArg_expr = OWL[]
  for j=1:length(ast.arg_expr)
    thing = analyze(ast.arg_expr[j])
    #println(thing)
    push!(newArg_expr, analyze(thing))
  end
  return FuncAppNode( analyze(ast.fun_expr), newArg_expr )#* arg_exp
end

function analyze(ast::If0Node)
  #println("analyze::If0Node")
  #println(ast)
  cond = analyze(ast.condition)
  return If0Node( cond, analyze(ast.zero_branch), analyze(ast.nonzero_branch) )
end



# calcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#the starter
function calc( owl::Any )
    return calc(owl, mtEnv())
end


function calc( owl::NumNode, env::Environment )
  #println("calc:NumNode")
  #println(owl.n)
  return NumVal(owl.n)
end

function calc(e::Unop, env::Environment)
  #println("calcUnop")
  #println(e)
  if e.op == collatz
    #println("Collatz check here!")
    numN = calc(e.operand)
    if (typeof(numN) != NumVal)
        throw( LispError("Unop Collatz: Isn't a number!") )
    end
    #println("numN", numN)
    if (numN.n <= 1)
      #println("Collatz error here!")
      throw( LispError("Collatz input must be bigger than 1!") )
    end
    answer = collatz(calc(e.operand))
    #println("collatz:",answer.n)
    return answer
  else #negate
    op1 = calc(e.operand, env)
    if (typeof(op1) != NumVal)
      throw( LispError("Unop negate: Isn't a number!") )
    end
    answer = e.op(op1.n)
    #println("negate",answer)
    return NumVal(answer)
 end
end


function calc(e::Binop, env::Environment)
  #println("calcBinop")
  #println(e)
  #3 expressions
  op1 = calc(e.lhs, env)
  op2 = calc(e.rhs, env)
  if (typeof(op1) != NumVal) || (typeof(op2) != NumVal)
    throw( LispError("Binop: One of these isn't a number!") )
  end
  answer = e.op(op1.n, op2.n)
  #/0
  if answer == Inf
    throw( LispError("Division by 0!") )
  end
  #println(answer)
  return NumVal(answer)
end

function calc( owl::If0Node, env::Environment  )
  #println("calc::If0Node")
  #println(owl)
  condition = calc( owl.condition, env )
  if (condition.n == 0)
    return calc(owl.zero_branch, env)
  else
    return calc(owl.nonzero_branch, env)
  end
end

function calc( owl::WithNode, env::Environment )
  #println("calc::WithNode")
  #println(owl)
  #println(env)

  throw(LispError("We should never get here anymore!"))
  return 0
  # #println("here is the new thing")
  # ext_env = env
  # for j=1:length(owl.binding_expr)
  #   new_binding_expr = owl.binding_expr[j]
  #   value = calc(new_binding_expr.owl, env)
  #   ext_env = ConcreteEnvironment(new_binding_expr.variable, value, ext_env)#lowers the pointer
  # end
  #
  # return calc(owl.body, ext_env)#return the rest of the functionality and the env
end

function calc( owl::IdNode , env::Environment )
  #println("calc::IdNode")
  #println(IdNode)
  if env == mtEnv()
    throw(LispError("Attempt to reference undefined variable: " * string(owl.name)))

  elseif owl.name == env.name
    return env.value

  else
    return calc( owl, env.parent )
  end
end

function calc( owl::FuncDefNode, env::Environment )
    #println("calc::FuncDefNode")
    #println("owl:")
    #println(owl)
    #println("env:")
    #println(env)

    return ClosureVal(owl.formal_parameter, owl.fun_body, env)
end

function calc( owl::FuncAppNode, env::Environment )
  #println("calc::FuncAppNode")
  #println("owl:")
  #println(owl)
  #println("env:")
  #println(env)
  the_closure = calc( owl.fun_expr, env )
  ext_env = the_closure.env#closure, child, child, child connection

  #the sizes must match
  #println("lengthsssssssssssssssssssssssssssssssssssssssssss")
  #println(length(owl.arg_expr))
  #println(length(the_closure.param))
  if ((length(owl.arg_expr)) != (length(the_closure.param)))
    throw(LispError("ClosureVal: Mismatch in owl.arg_exprs and closure.params!"))
  end

  for j=1:length(owl.arg_expr)
    actual_parameter = calc( owl.arg_expr[j], env )
    ext_env = ConcreteEnvironment(  the_closure.param[j], actual_parameter, ext_env)
  end

  return calc( the_closure.body, ext_env )
end



end # module
