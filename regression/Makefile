TESTS_ORIG=$(sort $(basename $(wildcard test???.expr)))
TESTS_Y=$(sort $(basename $(wildcard testy???.expr)))
TESTS=$(TESTS_ORIG) $(TESTS_Y)

.PHONY: check $(TESTS) list_tests check_orig check_y

check: $(TESTS) 
check_orig: $(TESTS_ORIG)
check_y: $(TESTS_Y)

list_tests:
	@echo $(TESTS)

$(TESTS): %: %.expr
	cat $@.input | ../src/rc.byte    $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log
	../src/rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f $(TESTS) *.log *.s
