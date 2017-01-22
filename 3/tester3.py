# example: run this with "python grader_filename.py your_code_filename"
# this is an auto-generated file for general student testing

import sys
import subprocess
import os
from difflib import Differ
if __name__ == "__main__":
    fn = sys.argv[1]
    tmp_fn = "tmp.rkt"
    feedback_fn = "feedback.txt"
    run_cmd = "racket"
    tests = """

(check-temps1 (list 80 92 56))
(check-temps1 (list 80 99 56))
(check-temps (list 80 92 56) 5 95)
(check-temps (list 80 99 56) 5 95)
(convert (list 1 2 3))
(duple (list 1 2 3))
(average (list 1 2 3 4))
(convertFC (list 32 50 212))
(eliminate-larger (list 1 2 3 9 4 5))
(get-nth (list 1 2 3 4) 2)
(find-item (list 1 2 3 4) 3)
(find-item (list 1 2 3 4) 42)
"""
    tests_info = """1. 1 point. check-temps1 general true test.
2. 1 point. check-temps1 general false test.
3. 1 point. check-temps general true test.
4. 1 point. check-temps general false test.
5. 1 point. convert general test.
6. 1 point. duple general test. Be careful when to use cons versus when to use list.
7. 1 point. average general test. You may want multiple helper functions. What are the sub-steps to computing an average?
8. 1 point. convertFC general test. Do you have the order of operations correct? What is the equation for the conversion?
9. 1 point. eliminate-larger general test. Hint: try using a helper function.
10. 1 point. get-nth general test. Remember, this is 0-based indexing.
11. 1 point. find-item general test. Hint: consider using an auxiliary variable.
12. 1 point. find-item general test for target does not exist.
"""
    correctoutput = """#t
#f
#t
#f
321
'((1 1) (2 2) (3 3))
5/2
'(0 10 100)
'(1 2 3 4 5)
3
2
-1
"""
    grade = 0
    total_possible = 0
    with open(fn, "r") as f:
        with open(tmp_fn, "w") as w:
            w.write(f.read())
            w.write(tests)
    cmd = [run_cmd, tmp_fn]
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    studentoutput, err = process.communicate()
    comparison = "".join(Differ().compare(correctoutput.splitlines(1), studentoutput.splitlines(1)))
    error_line_nos = []
    extra_line_nos = []
    q_line_nos = []
    for count, i in enumerate(comparison.splitlines()):
        if "-" == i[0]:
            error_line_nos.append(count)
        elif "+" == i[0]:
            extra_line_nos.append(count)
        elif "?" == i[0]:
            q_line_nos.append(count)
    failed_tests_line_nos = []
    for x in error_line_nos:
        numextralines = len([y for y in extra_line_nos if y < x])
        numqlines = len([z for z in q_line_nos if z < x])
        failed_tests_line_nos.append(x - numextralines - numqlines)
    with open(feedback_fn, "w") as feedback_file:
        feedback_file.write("        Correct output:\n")
        feedback_file.write(str(correctoutput))
        feedback_file.write("\n        Your output:\n")
        feedback_file.write(str(studentoutput))
        feedback_file.write("\n        Failed tests:\n")
        for count, l in enumerate(tests_info.splitlines(1)):
            points = int(l.split()[1])
            if count in failed_tests_line_nos:
                total_possible += points
                feedback_file.write(l)
            elif count in extra_line_nos:
                pass
            else:
                total_possible += points
                grade += points
        feedback_file.write("\n        Grade:\n" + str(grade) + " out of " + str(total_possible))
    os.remove(tmp_fn)
    print("See feedback file: " + feedback_fn)
