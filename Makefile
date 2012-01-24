BUILDDIR:=$(shell pwd)/build
TARGET:=Main

all: src/$(TARGET)

src/$(TARGET): src/*.hs src/Parser.hs src/Lexer.hs
	test -d $(BUILDDIR) || (rm -f $(BUILDDIR) && mkdir $(BUILDDIR))
	(cd src;ghc --make $(TARGET) -outputdir $(BUILDDIR))

src/Parser.hs:src/Parser.y
	happy --info $< -o $@

src/Lexer.hs:src/Lexer.x
	alex $< -o$@

test: src/$(TARGET)
	$< < examples/Simple.mdl

clean:
	rm -rf $(BUILDDIR) src/Lexer.hs src/Parser.{hs,info} src/$(TARGET)
