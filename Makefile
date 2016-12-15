CXX = g++
CXXFLAGS += -std=c++11 -O2 -fPIC
LDFLAGS += -lm

TARGET = ischeme
TARGET_HEADER = ischeme.h
TARGET_SO = libischeme.so
INSTALL_DIR = /usr/local
SRC = $(wildcard *.cc)
OBJ_DIR = .obj
OBJ = $(SRC:%.cc=$(OBJ_DIR)/%.o)
DEP = $(SRC:%.cc=$(OBJ_DIR)/%.d)

.PHONY: all clean install
all: $(TARGET) $(TARGET_SO)
$(TARGET): $(OBJ)
	$(CXX) -o $@ $^ $(LDFLAGS)
$(TARGET_SO): $(OBJ)
	$(CXX) -shared -o $@ $^ $(LDFLAGS)
$(OBJ): $(OBJ_DIR)/%.o : %.cc
	@mkdir -p $(OBJ_DIR)
	$(CXX) -c $< -o $@ $(CXXFLAGS)
$(DEP): $(OBJ_DIR)/%.d : %.cc
	@mkdir -p $(OBJ_DIR); \
	$(CXX) -MM $(CXXFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\$(OBJ_DIR)/$*.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$
-include $(DEP)
test: $(TARGET)
	@./$(TARGET) test/test.isc
yinyang: $(TARGET)
	@./$(TARGET) test/yinyang.isc
nqueens: $(TARGET)
	@./$(TARGET) test/nqueens.isc
clean:
	-rm $(TARGET) $(TARGET_SO) $(OBJ_DIR) -rf
install:
	cp -f $(TARGET) $(INSTALL_DIR)/bin
	cp -f $(TARGET_HEADER) $(INSTALL_DIR)/include
	cp -f $(TARGET_SO) $(INSTALL_DIR)/lib
