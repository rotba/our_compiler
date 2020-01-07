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
	rm -f *.log *.cache *TESTCASE_*

test:
	for test in $$(ls | grep -i ^TESTCASE_ | cut -f 1 -d '.'); do cd $(MKDIR) && ocaml compiler.ml $(BASEDIR)/$$test.scm > $$test.s && nasm -f elf64 -o $$test.o $$test.s && gcc -static -m64 -o $$test $$test.o  &&  ./$$test > ACTUAL_$$test.txt && python run_testcase.py ACTUAL_$$test.txt EXPECTED_$$test.txt ; done && make clean

old_test:
	ocaml ${TEST_TO_RUN} && make clean

test_all:
	for test in $$(ls | grep -i test_); do ocaml $$test; done && make clean

patch:
	git diff ${FRESH} ${FILES_TO_SUB} > ${PATCH}
