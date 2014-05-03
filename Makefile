all:
	happy -gca Paractualgrammar.y
	alex -g Lexactualgrammar.x
	latex Docactualgrammar.tex; dvips Docactualgrammar.dvi -o Docactualgrammar.ps
	ghc --make Testactualgrammar.hs -o Testactualgrammar
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docactualgrammar.ps
distclean: clean
	-rm -f Docactualgrammar.* Lexactualgrammar.* Paractualgrammar.* Layoutactualgrammar.* Skelactualgrammar.* Printactualgrammar.* Testactualgrammar.* Absactualgrammar.* Testactualgrammar ErrM.* SharedString.* actualgrammar.dtd XMLactualgrammar.* Makefile*



