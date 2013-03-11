debug:
	ghc -o thetest -prof -fprof-auto -rtsopts -O -XDataKinds -XTypeOperators -XConstraintKinds -package-db ../dist/package.conf.inplace Test.hs

all:
	ghc -o thetest -O -XDataKinds -XTypeOperators -XConstraintKinds -package-db ../dist/package.conf.inplace Test.hs

run:
	./thetest +RTS -hc -p  -RTS
	hp2ps -c thetest.hp

clean: 
	rm -f thetest
	rm -f *.o
	rm -f *.hi

