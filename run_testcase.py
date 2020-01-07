import sys
import subprocess
from os.path import join
def main():
        actual_path = sys.argv[1]
        expected_path = join('..',sys.argv[2])
        with open(actual_path, "r") as f:
		act = f.read().strip('\n')
	with open(expected_path, "r") as f:
		exp = f.read().strip('\n')
        if act==exp:
                print("all_good")
        else:
                print("fail")
          
if __name__== "__main__":
        main()
