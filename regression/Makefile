TESTS=$(sort $(patsubst %.expr,%,$(wildcard test???.expr)))

.PHONY: check $(TESTS)

check: $(TESTS) 

list_tests:
	@echo $(TESTS)

$(TESTS): %: %.expr
	cat $@.input | ../src/rc.byte    $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | ../src/rc.byte -s $< > $@.log && diff $@.log orig/$@.log
	../src/rc.byte -o $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
