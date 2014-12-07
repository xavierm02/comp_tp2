all: src/main.native

test: src/main.native
	src/main.native

clean:
	(cd src; ./clean.sh)

.PHONY: all test clean

src/main.native: src/*.ml
	(cd src; ./build.sh)
