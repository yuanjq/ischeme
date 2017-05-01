DEBUG = 1
CXX = g++
ifeq ($(DEBUG), 1)
CXXFLAGS += -std=c++11
else
CXXFLAGS += -std=c++11 -O2
endif
CLIBFLAGS = -fPIC
LDFLAGS += -lm

TARGET = ischeme
TARGET_HEADER = ischeme.h
TARGET_LIB_DIR = lib
TARGET_SO = libischeme.so
INSTALL_DIR = /usr/local
INSTALL_LIB_DIR = $(INSTALL_DIR)/lib/ischeme
CXXFLAGS += -DISC_LIB_DIR=\"$(INSTALL_LIB_DIR)\"
SRC =  ischeme.cc compiler.cc gc.cc macro.cc numeric.cc vm.cc
OBJ_DIR = .obj
OBJ = $(SRC:%.cc=$(OBJ_DIR)/%.o)
DEP = $(SRC:%.cc=$(OBJ_DIR)/%.d)

.PHONY: all clean install test yinyang nqueens
all: $(TARGET) $(TARGET_SO)
$(TARGET): $(TARGET_SO)
	$(CXX) -o $@ main.cc $(CXXFLAGS) $(LDFLAGS) -L. -lischeme
$(TARGET_SO): $(OBJ)
	$(CXX) -shared -o $@ $^ $(LDFLAGS)
$(OBJ): $(OBJ_DIR)/%.o : %.cc
	@mkdir -p $(OBJ_DIR)
	$(CXX) -c $< -o $@ $(CXXFLAGS) $(CLIBFLAGS)
$(DEP): $(OBJ_DIR)/%.d : %.cc
	@mkdir -p $(OBJ_DIR); \
	$(CXX) -MM $(CXXFLAGS) $(CLIBFLAGS) $< > $@.$$$$; \
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
	mkdir -p $(INSTALL_LIB_DIR)
	cp -f $(TARGET_LIB_DIR)/* $(INSTALL_LIB_DIR)
