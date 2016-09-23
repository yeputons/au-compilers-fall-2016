TESTS_ORIG=$(sort $(basename $(wildcard test???.expr)))
TESTS_Y=$(sort $(basename $(wildcard testy???.expr)))
TESTS=$(TESTS_ORIG) $(TESTS_Y)

.PHONY: check $(TESTS) list_tests test_orig test_y

check: $(TESTS) 

list_tests:
	@echo $(TESTS)

test_orig: $(TESTS_ORIG)
test_y: $(TESTS_Y)

$(TESTS): %: %.expr
	cat $@.input | ../src/rc.byte    $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log
	../src/rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f $(TESTS) *.log *.s
