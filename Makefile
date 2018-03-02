
main: test.o lib/io.o lib/numeric.o
	d16-ld $^ -o $@
%.d16: %.fs
	stack exec forthc-exe -- $< $@

%.o: %.d16
	d16 $< -o $@

.PHONY: run

run: main
	d16-jit $<
