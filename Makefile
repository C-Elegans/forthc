
main: test.o lib/io.o
	d16-ld $^ -o $@
%.asm: %.fs
	stack exec forthc-exe -- $< $@

%.o: %.asm
	d16 $< -o $@

