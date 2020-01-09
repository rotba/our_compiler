import sys
import subprocess
from os.path import join
def main():
        actual_path = sys.argv[1]
        expected_path = join('testcases',sys.argv[2])
        with open(actual_path, "r") as f:
		act = f.read().strip('\n')
	with open(expected_path, "r") as f:
		exp = f.read().strip('\n')
        if eq(exp, act):
                print("all_good")
        else:
                print("fail")
                print("expected: {}, got: {} ".format(exp, act))

def eq(exp, act):
        sub
          
if __name__== "__main__":
        main()
