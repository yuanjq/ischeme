.PHONY: ischeme clean
CFLAGS += --std=c99
LDFLAGS += -lm

ischeme: ischeme.c
	cc -o $@ $< $(CFLAGS) $(LDFLAGS)
clean:
	rm ischeme
