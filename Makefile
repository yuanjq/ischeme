.PHONY: ischeme test clean
CFLAGS += --std=c99
LDFLAGS += -lm

ischeme: ischeme.c compiler.c vm.c gc.c
	cc -o $@ $^ $(CFLAGS) $(LDFLAGS)
test:
	make && ./ischeme test/test.isc
yinyang:
	make && ./ischeme test/yinyang.isc
clean:
	rm ischeme
