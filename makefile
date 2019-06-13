CC=g++
CFLAGS=-Wall -Werror
ODIR = .obj

DEPS = ast.h lib/enums.h
_OBJ = main.o ast.o 

OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: %.cc $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

tako: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o tako
