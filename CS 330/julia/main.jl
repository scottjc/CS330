module main

push!(LOAD_PATH, ".")

using Error
using Lexer
using CITypes




import sys
import subprocess
import os
from difflib import Differ

function lexParse(str)
  CITypes.parse(Lexer.lex(str))
end

function parseInter(str)
  CITypes.type_of_expr(lexParse(str))
end

function removeNL(str)
  replace(string(str), "\n", "")
end

function testerr(f, param)
  try
    return removeNL(f(param))
  catch Y
    if (typeof(Y) != Error.LispError)
      return Y
    else
      return "Error"
    end
  end
end

#(with x 1 ( + x 2))
#((lambda x number (+ x 2)) 3)
#number : number



#println(CITypes.type_of_expr("(+ 1 1)"))
#println(CITypes.type_of_expr("(+ 1 (- 3 4))"))

#println(CITypes.type_of_expr("(ifb false true true)"))
#println(CITypes.type_of_expr("(ifb true 2 3)"))

#println(CITypes.type_of_expr("(with x (+ 5 1) (+ x x) )"))
#println(CITypes.type_of_expr("(with x 1 true)"))
#println(CITypes.type_of_expr("(with x 1 ( + x 2))"))

#println(CITypes.type_of_expr(" (lambda x : number (+ x 1))"))
#println(CITypes.type_of_expr("(lambda f : (number : number) (f 3))"))

#println(CITypes.type_of_expr(" nempty "))
#println(CITypes.type_of_expr("(ncons 9 (ncons 5 (ncons 6 nempty)))"))
#println(CITypes.type_of_expr("(nfirst (ncons 9 (ncons 54 nempty)))"))
#println(CITypes.type_of_expr("(nrest (ncons 9 (ncons 5 (ncons -2 (ncons 100 nempty)))))"))


println(testerr(parseInter, "true")) #CITypes.BoolType()
println(testerr(parseInter, "4")) #CITypes.NumType()
println(testerr(parseInter, "(+ 1 2)"))#CITypes.NumType()

println(testerr(parseInter, "(iszero nempty)"))#Error
println(testerr(parseInter, "(ifb false (ncons 1 nempty) nempty)"))#CITypes.NListType()

println(testerr(parseInter, "(with x 3 ( + 1 x))"))# CITypes.NumType()
println(testerr(parseInter, "(lambda x : number false)"))#CITypes.FunType(CITypes.NumType(),CITypes.BoolType())
println(testerr(parseInter, "((lambda x : number false) false)"))#error

println(testerr(parseInter, "nempty"))#CITypes.NListType()
println(testerr(parseInter, "(nisempty false)"))#Error
println(testerr(parseInter, "(nfirst (ncons 1 nempty))"))#CITypes.NumType()
println(testerr(parseInter, "(nrest nempty)"))#CITypes.NListType()


end #module
