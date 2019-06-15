DEPS = src/*.* src/**/*.*

build/Makefile: src/**/CMakeLists.txt
	cmake -Hsrc -Bbuild

build/tako: build/Makefile $(DEPS)
	make -C build tako

build/takoTest: build/Makefile $(DEPS)
	make -C build takoTest

tako: build/tako $(DEPS)
	cp build/tako tako

test: build/takoTest $(DEPS)
	./build/takoTest

.PHONY: clean test tako

clean:
	rm -rf build tako

parsetest: tako
	./tako test.tako
