TESTS_STACK=
TESTS_INTERPRET=testy024 testy025 testy026
TESTS_IGNORE=
TESTS_ORIG=$(filter-out $(TESTS_STACK) $(TESTS_INTERPRET) $(TESTS_IGNORE), $(sort $(basename $(wildcard test???.expr))))
TESTS_Y=$(filter-out $(TESTS_STACK) $(TESTS_INTERPRET) $(TESTS_IGNORE), $(sort $(basename $(wildcard testy???.expr))))
TESTS=$(TESTS_ORIG) $(TESTS_Y)

.PHONY: check $(TESTS) list_tests check_orig check_y check_interpreter

check: $(TESTS) $(TESTS_INTERPRET) $(TESTS_STACK)
check_orig: $(TESTS_ORIG)
check_y: $(TESTS_Y)
check_interpreter: $(TESTS_INTERPRET)
check_stack: $(TESTS_STACK)

list_tests:
	@echo $(TESTS)
	@echo $(TESTS_INTERPRET)
	@echo $(TESTS_STACK)

$(TESTS): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log
	RC_RUNTIME=../runtime ../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

$(TESTS_INTERPRET): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log

$(TESTS_STACK): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f $(TESTS) *.log *.s
