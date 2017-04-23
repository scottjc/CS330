module main

push!(LOAD_PATH, ".")

using Error
using HPLexer
using HPInt


macro tnum()
       return :(testnum += 1)
end

testnum = 0
function testNum(num)
  return string(num) * ". "
end

function parseT(str)
  HPInt.parse(HPLexer.lex(str))
end

function analyze(str)
  HPInt.analyze(parseT(str))
end

function interpret(str)
  HPInt.calc(analyze(str))
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
#testAns(interpret, "(+ 1 2 3)", @tnum()) #HPInt.NumVal(6) ok


#testAns(interpret, "(and 1 2)", @tnum()) #HPInt.NumVal(1) ok
#testPass(parseT, "(and 1 2 3 3)", @tnum()) #PASS ok
#testPass(parseT, "(and (and 1 2) a)", @tnum()) #PASS ok
#testAns(interpret, "(* (and 1 2 3) (+ 1 2 3))", @tnum())#HPInt.NumVal(6) ok
#testAns(interpret, "(and 2 (lambda (x) x))", @tnum())#HPInt.NumVal(1)  ok
#testContains(analyze, "(and 1 2 3)", r"And", @tnum())#false ok

#testAns(interpret, "((lambda () 4))", @tnum()) # numval 4 ok

#testPass(parseT, "(with ((a 1) (b 1)) (+ b a))", @tnum()) # Pass ok
#testAns(interpret, "(with ((a 7) (b 3) (fun 4)) (+ b a fun))", @tnum()) #HPInt.NumVal(14) ok
#testAns(interpret, "(with ((x 5)) (+ x 1))", @tnum())#HPInt.NumVal(6) ok
#testAns(interpret, "(with ((x 1) (y 3) (z 4)) (+ x y z))", @tnum())#HPInt.NumVal(8) ok
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
# testAns(interpret, "(and 1 2)", @tnum()) #HPInt.NumVal(1) ok
#testPass(parseT, "(and 1 2 3 3)", @tnum()) #PASS ok
# testPass(parseT, "(and (and 1 2) a)", @tnum()) #PASS ok
#testAns(interpret, "(* (and 1 2 3) (+ 1 2 3))", @tnum())#HPInt.NumVal(6) ok


#new tests!!!
#testAns(interpret, "(with ((cat (simple_load \"./cat_256.png\"))) (simple_save cat \"./emboss_cat.png\"))", @tnum())

#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save cat \"new_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (emboss cat) \"emboss_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (drop_shadow cat) \"drop_shadow_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (inner_shadow cat) \"inner_shadow_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (min 0.5 cat) \"min_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (max 0.5 cat) \"max_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (+ cat cat) \"cat^2.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (- cat cat) \"cat^0.5.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (- 1 cat) \"-1_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (* cat cat) \"test.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (min (render_text \"CAT\" 40 230) cat) \"cat_label2.png\"))")))))
#parallelization
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (* (- 1 cat) cat) \"other_cat.png\"))")))))
#println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex("(with ((cat (simple_load \"cat_256.png\"))) (simple_save (min (render_text (+ 2 3) 40 230) cat) \"cat_label2.png\"))")))))

  println(HPInt.calc(HPInt.analyze(HPInt.parse(HPLexer.lex( "(with ((base_img (render_text \"Hello\" 25 100))
     (swirl (simple_load \"cat_256.png\")))
     (with ((ds (drop_shadow base_img)))
         (with ((tmp4 (+ (* (+ (min ds base_img) (- 1 base_img)) base_img) (* (- 1 base_img) swirl) )))
             (with ((tmp5 (- 1 (emboss tmp4)))
                     (base_img2 (render_text \"world!\" 5 200)))
                 (with ((is (inner_shadow base_img2)))
                     (with ((tmp6 (max base_img2 (* (- 1 base_img2) is) )))
                         (with ( (output (min tmp5 tmp6 )) )
                             (simple_save output \"output.png\")
                         )
                     )
                 )
             )
         )
    )
 )" )))))



println("-------------------------------END-----------------------------------")


end #module
