CFLAGS += --std=c99
ischeme: 
	cc -o $@ ischeme.c $(CFLAGS) -lm

clean:
	rm ischeme
