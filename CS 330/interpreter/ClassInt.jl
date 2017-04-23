
# If you're getting errors about not being able to load this, you may
# need to add the current directory to the module load path:
#
# push!(LOAD_PATH, ".")

module ClassInt

using Error
export parse, calc

#
# ===================================================
#

# This is the abstract class for arithmetic expressions.
abstract OWL

type NumNode <: OWL
  n::Real
end

type Plus <: OWL
  lhs::OWL
  rhs::OWL
end

type Minus <: OWL
  lhs::OWL
  rhs::OWL
end

#
# ===================================================
#

function parse( expr::Real )
  return NumNode( expr ) # return a "NumNode" type object, with the "n" member set to "expr"
end

function parse( expr::Array{Any} )
  # should be an array of length 3 - something like "(+ lhs rhs)"

  op_symbol = expr[1]
  lhs = parse( expr[2] )
  rhs = parse( expr[3] )

  if op_symbol == :+
    return Plus( lhs, rhs )

  elseif op_symbol == :-
    return Minus( lhs, rhs )

  else
    throw( LispError("Whoa there!  Unknown operation!") )
  end
end

# the default case
function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end


#
# ==================================================
#

# just a number - return it!
function calc( owl::NumNode )
  return owl.n
end

# calc each side and combine
function calc( owl::Plus )
  left = calc( owl.lhs )
  right = calc( owl.rhs )
  return left + right
end

# calc each side and combine
function calc( owl::Minus )
  left = calc( owl.lhs )
  right = calc( owl.rhs )
  return left - right
end

end # module
