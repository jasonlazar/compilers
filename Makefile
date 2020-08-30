CXXFLAGS=-g -Wall -std=c++11 `$(LLVMCONFIG) --cxxflags`
LLVMCONFIG=llvm-config-9
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`
CXX=c++
# CXX=clang++-9
CFLAGS=-g

SRCDIR=src
BUILDDIR=build
TARGET=tony

.PHONY: default clean distclean

default: $(TARGET)

$(SRCDIR)/lexer.cpp: $(SRCDIR)/lexer.l
	flex -s -o $@ $<

$(BUILDDIR)/lexer.o: $(SRCDIR)/lexer.cpp $(SRCDIR)/lexer.hpp $(SRCDIR)/parser.hpp $(SRCDIR)/ast.hpp
	mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(SRCDIR)/parser.hpp $(SRCDIR)/parser.cpp: $(SRCDIR)/parser.y
	bison -dv -o $(SRCDIR)/parser.cpp $<

$(BUILDDIR)/parser.o: $(SRCDIR)/parser.cpp $(SRCDIR)/lexer.hpp $(SRCDIR)/ast.hpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

$(TARGET): $(BUILDDIR)/lexer.o $(BUILDDIR)/parser.o $(BUILDDIR)/printers.o $(BUILDDIR)/library.o $(BUILDDIR)/symbol.o $(BUILDDIR)/general.o $(BUILDDIR)/error.o $(BUILDDIR)/ast.o
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	$(RM) -r $(BUILDDIR)
	$(RM) $(SRCDIR)/lexer.cpp $(SRCDIR)/parser.cpp $(SRCDIR)/parser.hpp $(SRCDIR)/parser.output *.out *.ll *.s *.dot *.png

distclean: clean
	$(RM) tony
