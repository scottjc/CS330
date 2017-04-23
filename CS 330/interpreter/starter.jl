module main

push!(LOAD_PATH, ".")

using Error
using Lexer
using PATInt

macro tnum()
       return :(testnum += 1)
end
using Lexer
using Error

testnum = 0
function testNum(num)
  return string(num) * ". "
end

function parseT(str)
  PATInt.parse(Lexer.lex(str))
end

function interpretT(str)
  PATInt.calc(parseT(str))
end

function removeNL(str)
  replace(string(str), "\n", "")
end

function testErr(f, param, num)
  try
    println(testNum(num) *  removeNL(f(param)))
  catch Y
    if (typeof(Y) != Error.LispError)
      println(testNum(num) * removeNL(Y))
    else
      println(testNum(num) * "Error")
    end
  end
end

function testPass(f, param, num)
  try
    f(param)
    println(testNum(num) * "Pass")
  catch Y
    println(testNum(num) * removeNL(Y))
  end
end

function testAns(f, param, num)
  try
    println(testNum(num) *  removeNL(f(param)))
  catch Y
    println(testNum(num) * removeNL(Y))
  end
end


testPass(parseT, "(+ 1 (- 3 4))", @tnum()) # Pass RudInt
testErr(parseT, "(-)", @tnum()) # Error
testErr(parseT, "if0", @tnum()) # Error
testAns(interpretT, "(if0 1 2 3)", @tnum()) #Pass
testAns(interpretT, "(with ((x 1)) x)", @tnum()) # Pass
testPass(interpretT, "(lambda (x) x)", @tnum()) # Pass
testPass(parseT, "(1 2 3)", @tnum()) #Pass
testErr(interpretT, "(+ (lambda (x) x) 2)", @tnum()) #Error
testErr(interpretT, "(collatz -1)", @tnum()) #Error
testAns(interpretT, "((lambda () 4))", @tnum()) # numval 4

end #module
