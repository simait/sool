BUILDDIR:=$(shell pwd)/build

all: src/Main

src/Main: src/*.hs
	test -d $(BUILDDIR) || (rm -f $(BUILDDIR) && mkdir $(BUILDDIR))
	(cd src;ghc --make Main -outputdir $(BUILDDIR))

src/Parser.hs:src/Parser.y
	happy --info $< -o $@

src/Lexer.hs:src/Lexer.x
	alex $< -o$@

test: src/Main
	$< < examples/Simple.mdl

clean:
	rm -rf $(BUILDDIR) src/Lexer.hs src/Parser.hs
