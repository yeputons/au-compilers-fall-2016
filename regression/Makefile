TESTS_INTERPRET=test011
TESTS_ORIG=$(filter-out $(TESTS_INTERPRET), $(sort $(basename $(wildcard test???.expr))))
TESTS_Y=$(sort $(basename $(wildcard testy???.expr)))
TESTS=$(TESTS_ORIG) $(TESTS_Y)

.PHONY: check $(TESTS) list_tests check_orig check_y check_interpreter

check: $(TESTS)  $(TESTS_INTERPRET)
check_orig: $(TESTS_ORIG)
check_y: $(TESTS_Y)
check_interpreter: $(TESTS_INTERPRET)

list_tests:
	@echo $(TESTS)
	@echo $(TESTS_INTERPRET)

$(TESTS): %: %.expr
	cat $@.input | ../rc.native -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../rc.native -s $< > $@.log && diff $@.log orig/$@.log
	../rc.native -o  $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

$(TESTS_INTERPRET): %: %.expr
	cat $@.input | ../rc.native    $< > $@.log && diff $@.log orig/$@.log

clean:
	rm -f $(TESTS) *.log *.s
