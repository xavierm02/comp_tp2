all: src/main.native

test: src/main.native
	src/main.native

run: tmp/test
	tmp/test || true

clean:
	(cd src; ./clean.sh);
	rm -rf tmp

.PHONY: all test run clean

src/main.native: src/*.ml
	(cd src; ./build.sh)

tmp:
	mkdir tmp

tmp/test.vsl: tmp
	cat > tmp/test.vsl

tmp/test: src/main.native tmp/test.vsl tmp
	(cd src; ./compile.sh ../tmp/test)
