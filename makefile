EXE_NAME = HEY_BROTHER
TEST_NAME = test_sa
SUFFIX = ml
TEST_TO_RUN = ${TEST_NAME}.${SUFFIX}
FRESH = origin/fresh
PATCH = compiler.patch
FILES_TO_SUB = reader.ml tag-parser.ml semantic-analyser.ml readme.txt pc.ml




.PHONY:clean


clean:
	rm -f *.log *.cache

test:
	ocaml ${TEST_TO_RUN} && make clean

test_all:
	for test in $$(ls | grep -i test_); do ocaml $$test; done && make clean

patch:
	git diff ${FRESH} ${FILES_TO_SUB} > ${PATCH}
