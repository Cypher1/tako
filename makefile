CC=g++ -lc++_shared
CFLAGS=-Wall -Werror
DEPS = ast.h enums.h
OBJ = main.o ast.o 

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

tako: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean

clean:
	rm -f *.o tako a.out
