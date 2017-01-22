# example: run this with "python grader_filename.py YOUR_CODE.rkt"
# this is an auto-generated file for general student testing

import sys
import subprocess
if __name__ == "__main__":
    fn = sys.argv[1]
    diff_fn = "Correct_Output.txt"
    info_fn = "Tests_Info.txt"
    tmp_fn = "tmp.rkt"
    output_fn = "Your_Output.txt"
    feedback_fn = "feedback.txt"
    run_cmd = "racket"
    tests = """(define (round-n num place)
  (let ((power (expt 10 place)))
    (/ (round (* power num)) power)))
(sum-coins 1 1 1 1)
(round-n (degrees-to-radians 60) 3)
(sign 4)
(round-n (new-sin 60 'radians) 3)
"""
    tests_info = """1 point. sum-coins function general test.
1 point. degrees-to-radians function general test. Make sure to use the Racket literal for the value of pi.
1 point. sign function general test.
1 point. new-sin function general test.
"""
    diff = """41
1.047
1
-0.305
"""
    grade = 0
    total_possible = 0
    with open(info_fn, "w") as w:
        w.write(tests_info)
    with open(diff_fn, "w") as w:
        w.write(diff)
    with open(fn, "r") as f:
        with open(tmp_fn, "w") as w:
            w.write(f.read())
            w.write(tests)
    cmd = [run_cmd, tmp_fn]
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
    out, err = process.communicate()
    with open(output_fn, "w") as w:
        w.write(out)
    
	print
	print tests_info
	
	print "Correct Output:"
	print diff
	print "Your Output:"
	print out
	
    cmd = ["del", tmp_fn, output_fn, info_fn, diff_fn]
    subprocess.call(cmd, shell=True)
