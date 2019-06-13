CC=g++
CFLAGS=-Wall -Werror
ODIR = .obj

DEPS = src/ast.h lib/enums.h
_OBJ = main.o ast.o 

OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: src/%.cc $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

tako: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean test

clean:
	rm -f $(ODIR)/*.o tako

test: tako
	./tako test.tako
