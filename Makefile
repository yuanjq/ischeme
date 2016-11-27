CXX = g++
CXXFLAGS += -std=c++11
LDFLAGS += -lm

TARGET = ischeme
INSTALL_DIR = /usr/bin
SRC = $(wildcard *.cc)
OBJ_DIR = obj
OBJ = $(SRC:%.cc=$(OBJ_DIR)/%.o)

.PHONY: all test clean

$(TARGET): $(OBJ)
	$(CXX) -o $@ $^ $(LDFLAGS)
$(OBJ): $(OBJ_DIR)/%.o : %.cc
	@mkdir -p $(OBJ_DIR)
	$(CXX) -c $< -o $@ $(CXXFLAGS)
test: $(TARGET)
	@./$(TARGET) test/test.isc
yinyang: $(TARGET)
	@./$(TARGET) test/yinyang.isc
nqueens: $(TARGET)
	@./$(TARGET) test/nqueens.isc
clean:
	-rm $(TARGET) $(OBJ_DIR) -rf
install: $(TARGET)
	cp -f $(TARGET) $(INSTALL_DIR)
