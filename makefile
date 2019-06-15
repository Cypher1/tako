build/Makefile: src/**/CMakeLists.txt
	cmake -Hsrc -Bbuild

build/tako: build/Makefile src/**/*.*
	make -C build tako

tako: build/tako
	cp build/tako tako

build/takoTest: build/Makefile src/**/*.*
	make -C build takoTest

test: build/takoTest
	./build/takoTest

.PHONY: clean test

clean:
	rm -rf build tako

parsetest: tako
	./tako test.tako
