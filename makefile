build/Makefile: src/**/CMakeLists.txt
	mkdir -p build
	cd build
	cmake src

build/tako: build/Makefile src/**/*.*
	make -C build tako

tako: build/tako
	cp build/tako tako

build/test: build/Makefile src/**/*.*
	make -C build tacoTest

test: build/test
	./build/test

.PHONY: clean test

clean:
	rm -rf build tako

parsetest: tako
	./tako test.tako
