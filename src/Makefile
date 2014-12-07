all: main.native

test: main.native
	./$< examples/test.vsl

clean:
	./clean.sh

.PHONY: all test clean

main.native: *.ml
	./build.sh
