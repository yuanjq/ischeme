.PHONY: ischeme test clean
CFLAGS += --std=c99
LDFLAGS += -lm

ischeme: ischeme.c compiler.c vm.c gc.c
	cc -o $@ $^ $(CFLAGS) $(LDFLAGS)
test: ischeme
	@./ischeme test/test.isc
yinyang: ischeme
	@./ischeme test/yinyang.isc
nqueens: ischeme
	@./ischeme test/nqueens.isc
clean:
	rm ischeme
