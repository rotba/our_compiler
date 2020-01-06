import sys
import subprocess
def main():
    with open("tmp.txt", "r") as f:
        exp = f.read().strip('\n')
    act = sys.argv[1]
    if act==exp:
        print("all_good")
    else:
        print("fail")
          
if __name__== "__main__":
  main()
