build/Makefile: src/**/CMakeLists.txt
	cmake -S src -B build

build/tako: build/Makefile src/**/*.*
	make -C build tako

tako: build/tako
	cp build/tako tako

build/test: build/Makefile src/**/*.*
	make -C build test

test: build/test
	./build/test

.PHONY: clean test

clean:
	rm -rf build tako

parsetest: tako
	./tako test.tako
