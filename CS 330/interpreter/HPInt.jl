module HPInt
push!(LOAD_PATH, ".")

using Images
using Cairo
using Error
using HPLexer
export parse, calc, analyze, NumVal, ClosureVal, MatrixVal


#  The high performance primitives you must bind in.
# ==========================================================================

function simple_load( img_path::AbstractString )
  #println("simple_load function")
  im = Images.load( img_path );
  tmp = Images.permuteddimsview(channelview(im),(2,3,1));#big not valid permutation?!!!
  d = Images.data( ImageMeta(tmp) );
  #orig
  #tmp = Images.separate(im);
  #d = Images.data( tmp );

  d = d[:,:,1];  # just the r channel
  d = convert( Array{Float32,2}, d );
  return d
end

function simple_save( output::Array, img_path::AbstractString )#a matrix in
    #println("simple_save function")
    output[ output .> 1.0 ] = 1.0
    output[ output .< 0.0 ] = 0.0
    tmpc = convert( Array{UInt32,2}, floor(output*255.0) )
    tmp_output =  tmpc + tmpc*256 + tmpc*65536 + 0xff000000
    c2 = CairoImageSurface( transpose(tmp_output), Cairo.FORMAT_ARGB32 )
    write_to_png( c2, img_path)
    return 42
end

function render_text( text_str::AbstractString, xpos, ypos )
  #println("render_text function")

  data = Matrix{UInt32}( 256, 256 );
  c = CairoImageSurface( data, Cairo.FORMAT_ARGB32 );
  cr = CairoContext( c );

  set_source_rgb( cr, 1., 1., 1. );
  rectangle( cr, 0., 0., 256., 256. );
  fill( cr );

  set_source_rgb( cr, 0, 0, 0 );
  select_font_face( cr, "Sans", Cairo.FONT_SLANT_NORMAL,
                    Cairo.FONT_WEIGHT_BOLD );
  set_font_size( cr, 90.0 );

  move_to( cr, xpos, ypos );
  show_text( cr, text_str );

  # tmp is an Array{UInt32,2}
  tmp = cr.surface.data;

  # grab just the blue channel, and convert the array to an array of floats
  tmp2 = convert( Array{Float32,2}, tmp & 0x000000ff ) / 255.0;
  tmp2 = convert( Array{Float32,2}, tmp2 );

  return tmp2
end

function emboss( img::Array )
  #println("emboss function")
  f = [ -2. -1. 0.
        -1.  1. 1.
         0.  1. 1. ];
  f = convert( Array{Float32,2}, f );

  es = conv2( f, img );
  es = es[1:256,1:256];
  return es
end

function drop_shadow( img::Array )
  #println("drop_shadow function")
#foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
  foo = convert(Array{Float32,2}, Kernel.gaussian((5.0,5.0),(25,25))[-12:12,-12:12]);
  foo = foo / maximum(foo);
  ds = conv2( foo, img );
  ds = ds[13:256+12,13:256+12];
  ds = ds / sum(foo);
  return ds
end

# assumes img is black-on-white
function inner_shadow( img::Array )
  #println("inner_shadow function")
  #foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
  foo = convert(Array{Float32,2}, Kernel.gaussian((5.0,5.0),(25,25))[-12:12,-12:12]);
  foo = foo / maximum(foo);
  is = conv2( foo, 1.0-img );
  is = is[8:251+12,8:251+12];
  is = is / sum(foo);
  is = max( is, img );
  return is
end
# ============================================================================



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

#new stuff-------------------------------------------------------------

type simple_loadNode <: OWL
  string::AbstractString
end

type simple_saveNode <: OWL
  owl::OWL
  # this owl should evaluate to a MatrixVal
  string::AbstractString
end

#02/27/17
# (render_text "Hello" (+ 25 100) 400)

type render_textNode <: OWL
  the_words::AbstractString
  xpos::OWL
  ypos::OWL
   # these owls should evaluate to NumVals
 end


#only seen at calc time
type embossNode <: OWL
  owl::OWL
  # this owlshould evaluate to a MatrixVal
end

type drop_shadowNode <: OWL
  owl::OWL
  # this owlshould evaluate to a MatrixVal
end

type inner_shadowNode <: OWL
  owl::OWL
  # this owlshould evaluate to a MatrixVal
end


# RetValllllllllllllll and environmenttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
abstract RetVal
abstract Environment

type MatrixVal <: RetVal
  val::Array{Float32, 2}
end

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
  :collatz => collatz,
  #new functions the other stuf is a node
  :min => min,
  :max => max
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

  #looking for RunInt stuff #new binops
  if ((get(op_table, expr[1], 0) == +) || (get(op_table, expr[1], 0) == -) ||
        (get(op_table, expr[1], 0) == *) || (get(op_table, expr[1], 0) == /) ||
        (get(op_table, expr[1], 0) == mod) || (get(op_table, expr[1], 0) == emboss) ||
        (get(op_table, expr[1], 0) == drop_shadow) || (get(op_table, expr[1], 0) == inner_shadow) ||
        (get(op_table, expr[1], 0) == min) || (get(op_table, expr[1], 0) == max)) && (length(expr) == 3)

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
        #println("parse:Unop")
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

  #new stuff
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

 #new onesssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
 elseif expr[1] == :simple_load #these ok?
   #println("parse:simple_load")
   if (length(expr) != 2)
     throw(LispError("Something went wrong with simple_load! Not right length!"))
   end
   if (typeof(expr[2]) != String)
     throw(LispError("Something went wrong with simple_load! No string given!"))
   end
     return simple_loadNode( expr[2] )

 elseif expr[1] == :simple_save
   #println("parse:simple_save")
   #println(expr)
   if (length(expr) != 3)
     throw(LispError("Something went wrong with simple_save! Not right length!"))
   end
   if (typeof(expr[3]) != String)
     throw(LispError("Something went wrong with simple_save! No string given!"))
   end
     return simple_saveNode( parse(expr[2]), expr[3] )

 elseif expr[1] == :render_text
   #println("parse:render_text")
   if (length(expr) != 4)
     throw(LispError("Something went wrong with render_text! Not right length!"))
   end
   if (typeof(expr[2]) != String)
     throw(LispError("Something went wrong with render_text! No string given!"))
   end
   return render_textNode( expr[2], parse(expr[3]), parse(expr[4]) )

 elseif expr[1] == :emboss
   #println("parse:emboss")
   if (length(expr) != 2)
     throw(LispError("Something went wrong with emboss! Not right length!"))
   end

   return embossNode(parse(expr[2]))

 elseif expr[1] == :drop_shadow
   #println("parse:drop_shadow")
   if (length(expr) != 2)
     throw(LispError("Something went wrong with drop_shadow! Not right length!"))
   end

   return drop_shadowNode(parse(expr[2]))

 elseif expr[1] == :inner_shadow
   #println("parse:inner_shadow")
   if (length(expr) != 2)
     throw(LispError("Something went wrong with inner_shadow! Not right length!"))
   end

   return inner_shadowNode(parse(expr[2]))


 elseif expr[1] == :inner_shadow
   #println("parse:inner_shadow")
   if (length(expr) != 2)
     throw(LispError("Something went wrong with inner_shadow! Not right length!"))
   end

   return inner_shadowNode(parse(expr[2]))



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

#new analyze methodssssssssssssssssssssssssssssssssssssssssssssssss
function analyze(ast::simple_loadNode)
  #println("analyze::simple_loadNode")
  #println(ast)
  return ast
end

function analyze(ast::simple_saveNode)
  #println("analyze::simple_saveNode")
  #println(ast)
  return simple_saveNode( analyze(ast.owl), ast.string )
end

function analyze(ast::render_textNode)
  #println("analyze::render_textNode")
  #println(ast)
  return render_textNode( ast.the_words, analyze(ast.xpos), analyze(ast.ypos) )
end

function analyze(ast::embossNode)
  #println("analyze::embossNode")
  #println(ast)
  return embossNode(analyze(ast.owl))
end

function analyze(ast::drop_shadowNode)
  #println("analyze::drop_shadow")
  #println(ast)
  return drop_shadowNode(analyze(ast.owl))
end

function analyze(ast::inner_shadowNode)
  #println("analyze::inner_shadow")
  #println(ast)
  return inner_shadowNode(analyze(ast.owl))
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


    if (typeof(op1) == MatrixVal)#new
      #println("Doin' them matrix UNOP!")
      answer = e.op(op1.val)
      return MatrixVal(answer)
    end

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

  #threads!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #println("threads time!")
  op1 = @spawn calc(e.lhs, env)
  op2 = @spawn calc(e.rhs, env)

  op1 = fetch(op1)
  op2 = fetch(op2)

  #check for a closureval
  if(typeof(op1) == ClosureVal || typeof(op2) == ClosureVal)
    throw( LispError("NO CLOSUREVALS IN BINOP!!!!") )
  end


  if (typeof(op1) == MatrixVal && typeof(op2) == NumVal)#new
    #println("Doin' them matrix 1!")
    answer = 0
    if(e.op == *)
      #println("Multiply stuff 1")
      answer = (op1.val .* op2.n)
    else
      answer = e.op(op1.val, op2.n)
    end
    #/0
    if answer == Inf
      throw( LispError("Division by 0!") )
    end
    #println(answer)
    return MatrixVal(answer)
  end

  if (typeof(op2) == MatrixVal && typeof(op1) == NumVal)
    #println("Doin' them matrix 2!")
    answer = 0
    if(e.op == *)
      #println("Multiply stuff 2")
      answer = (op1.n .* op2.val)
    else
      answer = e.op(op1.n, op2.val)
    end
    #/0
    if answer == Inf
      throw( LispError("Division by 0!") )
    end
    #println(answer)
    return MatrixVal(answer)
  end

  if (typeof(op1) == MatrixVal) || (typeof(op2) == MatrixVal)
    #println("Doin' them matrix both!")
    answer = 0
    if(e.op == *)
      #println("Multiply stuff 3")
      answer = (op1.val .* op2.val)
    else
      answer = e.op(op1.val, op2.val)
    end
    #/0
    if answer == Inf
      throw( LispError("Division by 0!") )
    end
    #println(answer)
    return MatrixVal(answer)
  end

  if (typeof(op1) != NumVal) || (typeof(op2) != NumVal)
    throw( LispError("Binop: One of these isn't a number!") )
  end
  #println("regular")
  answer = 0
  if(e.op == *)
    #println("Multiply stuff 4")
    answer = (op1.n .* op2.n)
  else
    answer = e.op(op1.n, op2.n)
  end
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
  #println(owl)
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

  the_closure = calc( owl.fun_expr, env )#parallel args!!!!!!!!!!!!!!!slids pmap with
  ext_env = the_closure.env#closure, child, child, child connection

  #the sizes must match
  #println("lengthsssssssssssssssssssssssssssssssssssssssssss")
  #println(length(owl.arg_expr))
  #println(length(the_closure.param))
  if ((length(owl.arg_expr)) != (length(the_closure.param)))
    throw(LispError("ClosureVal: Mismatch in owl.arg_exprs and closure.params!"))
  end

  #println("FuncAppNode threads time!")

  actual_parameters = pmap(x -> calc( x, env) ,  owl.arg_expr)

  for i=1:length(actual_parameters)
  # for i=1:length(owl.arg_expr)
    ext_env = ConcreteEnvironment(  the_closure.param[i], actual_parameters[i], ext_env)
  end

  return calc( the_closure.body, ext_env )
end

#new calcs
function calc(owl::simple_loadNode, env::Environment)
  #println("calc::simple_loadNode")
  #println(owl)
  return MatrixVal(simple_load(owl.string))
end

function calc(owl::simple_saveNode, env::Environment)
  #println("calc::simple_saveNode")
  #println(owl)
  matrix = calc(owl.owl, env)
  #println(matrix)
  simple_save(matrix.val, owl.string)
  #void
  return
end

function calc(owl::render_textNode, env::Environment)
  #println("calc::render_textNode")
  #println(owl)

  #threads
  #println("render_text threads!")
  xpos = @spawn calc(owl.xpos, env)
  ypos = @spawn calc(owl.ypos, env)

  xpos = fetch(xpos)
  ypos = fetch(ypos)

  return MatrixVal(render_text( owl.the_words, xpos.n, ypos.n))
end

function calc(owl::embossNode, env::Environment)
  #println("calc::embossNode")
  #println(owl)
  matrix = calc(owl.owl, env)
  return MatrixVal(emboss(matrix.val))
end

function calc(owl::drop_shadowNode, env::Environment)
  #println("calc::drop_shadowNode")
  #println(owl)
  matrix = calc(owl.owl, env)
  return MatrixVal(drop_shadow(matrix.val))
end

function calc(owl::inner_shadowNode, env::Environment)
  #println("calc::inner_shadowNode")
  #println(owl)
  matrix = calc(owl.owl, env)
  return MatrixVal(inner_shadow(matrix.val))
end

end # module
