module main

push!(LOAD_PATH, ".")

using Error
using Lexer
using TransInt

macro tnum()
       return :(testnum += 1)
end

macro tnum()
       return :(testnum += 1)
end

testnum = 0
function testNum(num)
  return string(num) * ". "
end

function parseT(str)
  TransInt.parse(Lexer.lex(str))
end

function analyze(str)
  TransInt.analyze(parseT(str))
end

function interpret(str)
  TransInt.calc(analyze(str))
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

function testContains(f, param, reg, num)
  try
    answer = string(ismatch(reg, removeNL(f(param))))
    println(testNum(num) * answer)
  catch Y
    println(testNum(num) * removeNL(Y))
  end
end


println("-----------------------------START-----------------------------------")
#testPass(parseT, "(+ 1 (- 3 4))", @tnum()) # Pass RudInt ok
#testPass(parseT, "(+ 1 2 3 4)", @tnum()) #PASS ok
#testPass(parseT, "(+ (+ 1 2 3) 2)", @tnum()) #PASS ok
#testAns(interpret, "(+ 1 2 3)", @tnum()) #TransInt.NumVal(6) ok


#testAns(interpret, "(and 1 2)", @tnum()) #TransInt.NumVal(1) ok
#testPass(parseT, "(and 1 2 3 3)", @tnum()) #PASS ok
#testPass(parseT, "(and (and 1 2) a)", @tnum()) #PASS ok
#testAns(interpret, "(* (and 1 2 3) (+ 1 2 3))", @tnum())#TransInt.NumVal(6) ok
#testAns(interpret, "(and 2 (lambda (x) x))", @tnum())#TransInt.NumVal(1)  ok
#testContains(analyze, "(and 1 2 3)", r"And", @tnum())#false ok

#testAns(interpret, "((lambda () 4))", @tnum()) # numval 4 ok

#testPass(parseT, "(with ((a 1) (b 1)) (+ b a))", @tnum()) # Pass ok
#testAns(interpret, "(with ((a 7) (b 3) (fun 4)) (+ b a fun))", @tnum()) #TransInt.NumVal(14) ok
#testAns(interpret, "(with ((x 5)) (+ x 1))", @tnum())#TransInt.NumVal(6) ok
#testAns(interpret, "(with ((x 1) (y 3) (z 4)) (+ x y z))", @tnum())#TransInt.NumVal(8) ok
#testContains(analyze, "(with ((x 1)) x)", r"with", @tnum())#false ok

#other tests
#testPass(parseT, "(+ 1 (- 3 4))", @tnum()) # Pass RudInt ok
#testErr(parseT, "(-)", @tnum()) # Error ok
#testErr(parseT, "if0", @tnum()) # Error ok
#testAns(interpret, "(if0 1 2 3)", @tnum()) #Pass ok
#testAns(interpret, "(with ((x 1)) x)", @tnum()) # Pass ok
#testPass(interpret, "(lambda (x) x)", @tnum()) # Pass ok
#testPass(parseT, "(1 2 3)", @tnum()) #Pass ok
#testErr(interpret, "(+ (lambda (x) x) 2)", @tnum()) #Error ok
#testErr(interpret, "(collatz -1)", @tnum()) #Error ok

#The tests to get all the points
# testAns(interpret, "(and 1 2)", @tnum()) #TransInt.NumVal(1) ok
# testPass(parseT, "(and 1 2 3 3)", @tnum()) #PASS ok
# testPass(parseT, "(and (and 1 2) a)", @tnum()) #PASS ok
# testAns(interpret, "(* (and 1 2 3) (+ 1 2 3))", @tnum())#TransInt.NumVal(6) ok
# testAns(interpret, "(and 2 (lambda (x) x))", @tnum())#TransInt.NumVal(1)  ok
# testContains(analyze, "(and 1 2 3)", r"And", @tnum())#false ok
#testAns(interpret, "(and 0 2)", @tnum()) #TransInt.NumVal(0) ok

println("-----------------------------END-----------------------------------")


end #module
