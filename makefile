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

%:
	cd $(MKDIR) && ocaml compiler.ml $(BASEDIR)/$@.scm > $@.s && nasm -f elf64 -o $@.o $@.s && gcc -static -m64 -o $@ $@.o && mv $@ $(BASEDIR)

clean:
	rm -f *.log *.cache *TESTCASE*

test:
	for test in $$(ls our_compiler/testcases/ | grep -i ^TESTCASE_ | cut -f 1 -d '.');\
	do\
		cd $(MKDIR) ;\
		ocaml our_compiler.ml $(BASEDIR)/our_compiler/testcases/$$test.scm > $$test.s ;\
		nasm -f elf64 -o $$test.o $$test.s ;\
		gcc -static -m64 -o $$test $$test.o ;\
		ACTUAL=`./$$test`;\
		export ACTUAL;\
		EXCPECTED=`scheme -q < testcases/$$test.scm`;\
		export EXCPECTED;\
		echo "(equal? '$$EXCPECTED '$$ACTUAL)" > test.scm;\
		scheme -q < test.scm;\
	done;\
	make -f test_makefile clean;

test_no_clean:
	for test in $$(ls our_compiler/testcases/ | grep -i ^TESTCASE_ | cut -f 1 -d '.'); do cd $(MKDIR) && ocaml our_compiler.ml $(BASEDIR)/our_compiler/testcases/$$test.scm > $$test.s && nasm -f elf64 -o $$test.o $$test.s && gcc -static -m64 -o $$test $$test.o  &&  ./$$test > ACTUAL_$$test.txt && python run_testcase.py ACTUAL_$$test.txt EXPECTED_$$test.txt our_compiler/testcases/; done 

old_test:
	ocaml ${TEST_TO_RUN} && make clean

test_all:
	for test in $$(ls | grep -i test_); do ocaml $$test; done && make clean

patch:
	git diff ${FRESH} ${FILES_TO_SUB} > ${PATCH}
