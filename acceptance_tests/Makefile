
.PHONY: check
check: local-tests basic-tests migrations-tests

.PHONY: local-tests
local-tests:
	./mzb_local_tests.py -v

.PHONY: basic-tests
basic-tests:
	./check_default_conf.erl
	./mzb_basic_tests.py -v --process-timeout=1200

.PHONY: migrations-tests
migrations-tests:
	./mzb_migrations_tests.py -v

.PHONY: quick-test
quick-test:
	./mzb_quick_test.py -v

.PHONY: clean
clean:
	-@rm -rf data.*
	-@rm -rf log.*
