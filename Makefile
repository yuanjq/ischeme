.PHONY: ischeme test clean
CFLAGS += --std=c99
LDFLAGS += -lm

ischeme: ischeme.c compiler.c vm.c gc.c
	cc -o $@ $^ $(CFLAGS) $(LDFLAGS)
test:
	make && ./ischeme test.isc
clean:
	rm ischeme
