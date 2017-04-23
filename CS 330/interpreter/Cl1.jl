
module CI1

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, interp


#
# ===================================================
#

abstract OWL

type NumNode <: OWL
  n::Real
end

type PlusNode <: OWL
  lhs::OWL
  rhs::OWL
end

type MinusNode <: OWL
  lhs::OWL
  rhs::OWL
end

#
# ===================================================
#

function parse( expr::Any )
  error("Bad programmer! Bad!")
end

function parse( expr::Real )
  return NumNode( expr )
end

function parse( expr::Symbol )
  return SymbolNode( expr )
end

function parse( expr::Array{Any} )
  # should hand us an array, should be length 3

  if expr[1] == :+
    return PlusNode( parse(expr[2]), parse(expr[3]) )

  elseif expr[1] == :-
    return MinusNode( parse(expr[2]), parse(expr[3]) )

end

#
# ===================================================
#

function interp( cs::AbstractString )
  lxd = Lexer.lex( cs )
  ast = parse( lxd )
  return calc( ast, mtEnv() )
end

#
# ===================================================
#

function calc( owl::NumNode, env::Environment )
  return owl.n
end

function calc( owl::PlusNode, env::Environment )
  return calc( owl.lhs, env ) + calc( owl.rhs, env )
end

function calc( owl::MinusNode, env::Environment  )
  return calc(owl.lhs,env) - calc(owl.rhs,env)
end

end # module
