EXE_NAME = HEY_BROTHER
TEST_NAME = test_sa
SUFFIX = ml
TEST_TO_RUN = ${TEST_NAME}.${SUFFIX}




.PHONY:clean


clean:
	rm -f *.log *.cache

test:
	ocaml ${TEST_TO_RUN} && make clean
