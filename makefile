DEPS = src/*.* src/**/*.*

tako: build/tako $(DEPS)
	cp build/tako tako

test: build/takoTest $(DEPS)
	./build/takoTest

build/Makefile: src/**/CMakeLists.txt
	cmake -Hsrc -Bbuild -DCMAKE_EXPORT_COMPILE_COMMANDS=1
	cp build/compile_commands.json .

build/tako: build/Makefile $(DEPS)
	make -C build tako

build/takoTest: build/Makefile $(DEPS)
	make -C build takoTest

.PHONY: clean test tako

clean:
	rm -rf build tako

parsetest: tako
	./tako test.tako -s Check
