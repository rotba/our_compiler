EXE_NAME = HEY_BROTHER
TEST_NAME = test_sa
SUFFIX = ml
TEST_TO_RUN = ${TEST_NAME}.${SUFFIX}
FRESH = origin/fresh
PATCH = compiler.patch
FILES_TO_SUB = reader.ml tag-parser.ml semantic-analyser.ml readme.txt pc.ml
MKDIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
BASEDIR := $(PWD)


.PHONY:clean


clean:
	rm -f *.log *.cache

test:
	cd $(MKDIR) && ocaml compiler.ml $(BASEDIR)/foo_1.scm > foo_1.s && nasm -f elf64 -o foo_1.o foo_1.s && gcc -static -m64 -o foo_1 foo_1.o  &&  ./foo_1 > tmp.txt && python run_testcase.py 1 && make clean

old_test:
	ocaml ${TEST_TO_RUN} && make clean

test_all:
	for test in $$(ls | grep -i test_); do ocaml $$test; done && make clean

patch:
	git diff ${FRESH} ${FILES_TO_SUB} > ${PATCH}
