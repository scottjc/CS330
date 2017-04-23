module RudInt
#types
#parse
#calc
using Error
export parse, calc, Num, Binop


abstract OWL

type Num <: OWL
	n::Real
end

type Binop <: OWL
	op::Function
	lhs::OWL
	rhs::OWL
end

type Unop <: OWL
	op::Function
	operand::OWL
end

function collatz( n::Real )
  return collatz_helper( n, 0 )
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


#parse ------------------------------------------
function parse(expr::Any)
	throw( LispError("Bad programmer! Bad!") )
end

function parse(expr::Real)#if we put in 42, we just get 42
	#println("Base.parseReal")
	#println(expr)
  return Num(expr)
end

function parse(expr::Array{Any})
  #a negative number or collatz
	#println("Base.parseArray{Any}")
	#println(expr)
  #println( length(expr) )

  if length(expr) == 3
		#println("3 expressions")
    try
			#println( expr[1], expr[2], expr[3] )
			#println(get(op_table, expr[1], 0))
      return Binop( get(op_table, expr[1], 0), parse(expr[2]), parse(expr[3]) )
    catch
			#println(LOAD_PATH)
      throw( LispError("Something went wrong with 3 expressions!") )
    end
	elseif length(expr) == 2
			#println("2 expressions")
			try
				#println(get(op_table, expr[1], 0))
				#println(expr[2])
				thing = Unop( get(op_table, expr[1], 0) , parse(expr[2]))
				#println("here in 2")
				return thing
			catch
				#println(LOAD_PATH)
				throw( LispError("Something went wrong with 2 expressions!") )
			end
	else
			throw( LispError("Not a recognized number of operators and operands!") )
	end
end




#calc--------------------------------------
#handles + - and a number
function calc(num::Num)
	#println("calcNum")
	#println(num)
  return num.n
end

function calc(e::Unop)
  #println("calcUnop")
	#println(e)
	if e.op == collatz
		answer = collatz(calc(e.operand))
		#println("collatz:",answer)
		return answer

 else #negate
	 op1 = calc(e.operand)
	 answer = e.op(op1)
	 #println("negate",answer)
	 return answer
 end
end


function calc(e::Binop)
  #println("calcBinop")
	#println(e)
	#3 expressions
	op1 = calc(e.lhs)
	op2 = calc(e.rhs)
	answer = e.op(op1, op2)
	#/0
	if answer == Inf
		throw( LispError("Division by 0") )
	end
	#println(answer)
	return answer
end

end #module
