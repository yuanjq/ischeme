.PHONY: ischeme test clean
CFLAGS += -O2 --std=c99
LDFLAGS += -lm

TARGET = ischeme
INSTALL_DIR = /usr/bin
SRC = ischeme.c compiler.c vm.c gc.c


$(TARGET): $(SRC)
	cc -o $@ $^ $(CFLAGS) $(LDFLAGS)
test: $(TARGET)
	@./$(TARGET) test/test.isc
yinyang: $(TARGET)
	@./$(TARGET) test/yinyang.isc
nqueens: $(TARGET)
	@./$(TARGET) test/nqueens.isc
clean:
	rm $(TARGET)
install: $(TARGET)
	cp -f $(TARGET) $(INSTALL_DIR)
