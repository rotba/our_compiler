EXE_NAME = HEY_BROTHER
TEST_NAME = test_sa
SUFFIX = ml
TEST_TO_RUN = ${TEST_NAME}.${SUFFIX}
FRESH = origin/freshh
PATCH = compiler.patch
FILES_TO_SUB = reader.ml tag-parser.ml semantic-analyser.ml readme.txt pc.ml code-gen.ml compiler.ml compiler.s prims.s stdlib.scm
MKDIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
BASEDIR := $(PWD)



.PHONY:clean

%:
	cd $(MKDIR) && ocaml compiler.ml $(BASEDIR)/$@.scm > $@.s && nasm -g -f elf64 -o $@.o $@.s && gcc -g -static -m64 -o $@ $@.o && mv $@ $(BASEDIR)

clean:
	rm -f *.log *.cache *TESTCASE*

cdclean:
	cd $(MKDIR) ;\
	rm -f *.log *.cache *TESTCASE*

test_ours:
	for test in $$(ls our_compiler/testcases/ours | grep -i ^TESTCASE_ | cut -f 1 -d '.');\
	do\
		cd $(MKDIR) ;\
		ocaml compiler.ml $(BASEDIR)/our_compiler/testcases/ours/$$test.scm > $$test.s ;\
		nasm -g -f elf64 -o $$test.o $$test.s ;\
		gcc -g -static -m64 -o $$test $$test.o ;\
		echo "testcase: $$test";\
		ACTUAL=`./$$test`;\
		export ACTUAL;\
		EXCPECTED=`scheme -q < testcases/ours/$$test.scm`;\
		export EXCPECTED;\
		echo "(if (equal? '($$EXCPECTED) '($$ACTUAL)) 'PASSED! '(Excpected Result: $$EXCPECTED,     Actual Result: $$ACTUAL) )" > test.scm;\
		scheme -q < test.scm;\
	done;\
	#make -f makefile clean;

test_ly:
	for test in $$(ls our_compiler/testcases/last_year | grep -i ^TESTCASE_ | cut -f 1 -d '.');\
	do\
		cd $(MKDIR) ;\
		ocaml compiler.ml $(BASEDIR)/our_compiler/testcases/last_year/$$test.scm > $$test.s ;\
		nasm -g -f elf64 -o $$test.o $$test.s ;\
		gcc -g -static -m64 -o $$test $$test.o ;\
		echo "testcase: $$test";\
		ACTUAL=`./$$test`;\
		export ACTUAL;\
		EXCPECTED=`scheme -q < testcases/last_year/$$test.scm`;\
		export EXCPECTED;\
		echo "(if (equal? '($$EXCPECTED) '($$ACTUAL)) 'PASSED! '(Excpected Result: $$EXCPECTED,     Actual Result: $$ACTUAL,    testcase: $$test) )" > test.scm;\
		scheme -q < test.scm;\
	done;\
	#make -f makefile clean;

test_no_clean:
	for test in $$(ls our_compiler/testcases/ | grep -i ^TESTCASE_ | cut -f 1 -d '.'); do cd $(MKDIR) && ocaml our_compiler.ml $(BASEDIR)/our_compiler/testcases/$$test.scm > $$test.s && nasm -f elf64 -o $$test.o $$test.s && gcc -static -m64 -o $$test $$test.o  &&  ./$$test > ACTUAL_$$test.txt && python run_testcase.py ACTUAL_$$test.txt EXPECTED_$$test.txt our_compiler/testcases/; done 

old_test:
	ocaml ${TEST_TO_RUN} && make clean

test_all:
	for test in $$(ls | grep -i test_); do ocaml $$test; done && make clean

patch:
	git diff ${FRESH} ${FILES_TO_SUB} > ${PATCH}
