BIN=bin
BOOTDIR=ghc-boot
OUTDIR=ghc-out
TOOLS=Tools
PROF= #-prof -fprof-auto
EXTS= -XScopedTypeVariables -XQualifiedDo -XTupleSections
GHCB=ghc $(PROF) -outputdir $(BOOTDIR)
GHCFLAGS=-i -ighc -ilib -i$(BOOTDIR) -hide-all-packages -XNoImplicitPrelude $(EXTS) -F -pgmF $(TOOLS)/convertY.sh 
GHCC=$(GHCB) $(GHCFLAGS)
GHC=ghc
# $(CURDIR) might not be quite right
GHCE=$(GHC) $(EXTS) -package mtl -F -pgmF Tools/convertX.sh -outputdir $(OUTDIR)
GCC=gcc
UPX=upx
ALLSRC=src/*/*.hs lib/*.hs lib/*/*.hs ghc/*.hs ghc/*/*.hs
MHS=mhs
COMB=comb/
EVAL=$(BIN)/eval
EVALSRC=src/runtime
EVALINC=include
.PHONY: all alltest everytest runtest bootboottest bootcombtest $(MHS)test test alltest time example bootstraptest

all:	$(EVAL) $(BIN)/$(MHS)

everytest:	runtest example examplecomb bootboottest bootcombtest

###
### Build evaluator (runtime system)
###
# On MINGW you might need the additional flags -Wl,--stack,50000000 to increase stack space.
$(EVAL):	$(EVALSRC)/*.c $(EVALINC)/*.h
	@mkdir -p bin
	$(GCC) -Wall -O3 -I$(EVALINC) $(EVALSRC)/node.c $(EVALSRC)/err.c $(EVALSRC)/parse.c $(EVALSRC)/bfile.c $(EVALSRC)/gc.c $(EVALSRC)/eval.c -o $(EVAL)

###
### Build the compiler with ghc, using standard libraries (Prelude, Data.List, etc)
###
$(BIN)/$(MHS):	src/*.hs src/*/*.hs $(TOOLS)/convertX.sh
	$(GHCE) -ighc -isrc -Wall -O src/MicroHs/Main.hs -main-is MicroHs.Main -o $(BIN)/$(MHS)

###
### Build the compiler with ghc, using MicroHs libraries (Prelude, Data.List, etc)
###
# Due to a ghc bug we need to list all the commands.
# The bug is that OPTIONS_GHC does not accept the -package flag.
$(BIN)/boot$(MHS):	$(ALLSRC) $(TOOLS)/convertY.sh
	rm -rf $(BOOTDIR)
	$(GHCB) -c ghc/Primitives.hs
	$(GHCB) -c ghc/Data/Bool_Type.hs
	$(GHCB) -c ghc/Data/Ordering_Type.hs
	$(GHCB) -c ghc/Data/Double.hs
	$(GHCB) -c src/PrimTable.hs
	$(GHCC) -c lib/Control/Error.hs
	$(GHCC) -c lib/Data/Bool.hs
	$(GHCC) -c lib/Data/Int.hs
	$(GHCC) -c lib/Data/Double.hs
	$(GHCC) -c lib/Data/Char.hs
	$(GHCC) -c lib/Data/Either.hs
	$(GHCC) -c lib/Data/Tuple.hs
	$(GHCC) -c lib/Data/Function.hs
	$(GHCC) -c lib/Data/Maybe.hs
	$(GHCC) -c lib/Data/Ord.hs
	$(GHCC) -c lib/Data/List.hs
	$(GHCC) -c lib/Text/String.hs
	$(GHCC) -c lib/Data/Word.hs
	$(GHCC) -c lib/System/IO.hs
	$(GHCC) -c lib/System/Environment.hs
	$(GHCC) -c lib/Prelude.hs
	$(GHCC) -c lib/PreludeNoIO.hs
	$(GHCC) -c lib/Data/Map.hs
	$(GHCC) -c lib/Data/IntMap.hs
	$(GHCC) -c lib/Data/IntSet.hs
	$(GHCC) -c lib/Unsafe/Coerce.hs
	$(GHCC) -c lib/Data/Integer.hs
	$(GHCC) -c lib/Control/Monad/State/Strict.hs
	$(GHCC) -c lib/Control/DeepSeq.hs
#	$(GHCC) -c lib/Debug/Trace.hs
	$(GHCC) -c lib/Control/Exception.hs
	$(GHCC) -c src/System/Console/SimpleReadline.hs
	$(GHCC) -c src/Text/ParserComb.hs
	$(GHCC) -c src/MicroHs/Ident.hs
	$(GHCC) -c src/MicroHs/Expr.hs
	$(GHCC) -c src/MicroHs/Graph.hs
	$(GHCC) -c src/MicroHs/Lex.hs
	$(GHCC) -c src/MicroHs/Parse.hs
	$(GHCC) -c src/MicroHs/IdentMap.hs
	$(GHCC) -c src/MicroHs/Exp.hs
	$(GHCC) -c src/MicroHs/TCMonad.hs
	$(GHCC) -c src/MicroHs/TypeCheck.hs
	$(GHCC) -c src/MicroHs/Desugar.hs
	$(GHCC) -c src/MicroHs/StateIO.hs
	$(GHCC) -c src/MicroHs/Compile.hs
	$(GHCC) -c src/MicroHs/Translate.hs
	$(GHCC) -c src/MicroHs/Interactive.hs
	$(GHCC) -c -main-is MicroHs.Main src/MicroHs/Main.hs
	$(GHC) $(PROF) -hide-all-packages -package time -o $(BIN)/boot$(MHS) $(BOOTDIR)/*.o $(BOOTDIR)/*/*.o $(BOOTDIR)/*/*/*.o $(BOOTDIR)/*/*/*/*.o
#	$(GHC) $(PROF) -hide-all-packages -package containers -o $(BIN)/boot$(MHS) $(BOOTDIR)/*.o $(BOOTDIR)/*/*.o $(BOOTDIR)/*/*/*/*.o

# Self compile using comb/mhs.comb
$(COMB)$(MHS)-new.comb: $(EVAL)
	$(EVAL) +RTS -r$(COMB)$(MHS).comb -RTS -ilib -isrc -o$(COMB)$(MHS)-new.comb MicroHs.Main

# Compare version compiled with normal GHC libraries and $(MHS) libraries
bootboottest:	$(BIN)/$(MHS) $(BIN)/boot$(MHS)
	$(BIN)/$(MHS)     -ilib -isrc -omain-$(MHS).comb  MicroHs.Main
	$(BIN)/boot$(MHS) -ilib -isrc -omain-boot.comb MicroHs.Main
	cmp main-$(MHS).comb main-boot.comb

# Compare version compiled with GHC, and bootstrapped combinator version
bootcombtest:	$(BIN)/$(MHS) $(EVAL) $(COMB)$(MHS).comb
	$(BIN)/$(MHS) -ilib -isrc -omain-$(MHS).comb  MicroHs.Main
	$(EVAL) +RTS -v -r$(COMB)$(MHS).comb -RTS -ilib -isrc -omain-comb.comb MicroHs.Main
	cmp main-$(MHS).comb main-comb.comb

###
### Run test examples with ghc-compiled compiler
###
runtest:	$(EVAL) $(BIN)/$(MHS) tests/*.hs
	cd tests; make test

###
### Run test examples with MicroHs compiler
###
runtestcomb: $(EVAL) $(COMB)$(MHS).comb
	cd tests; make MHS='../$(EVAL) +RTS -r../$(COMB)$(MHS).comb -RTS -i../lib'

###
### Build combinator file for the compiler, using ghc-compiled compiler
###
$(COMB)$(MHS).comb:	$(BIN)/$(MHS) $(ALLSRC)
	$(BIN)/$(MHS) -ilib -isrc -o$(COMB)$(MHS).comb MicroHs.Main

time:	$(EVAL) $(BIN)/$(MHS) tests/*.hs
	cd tests; make time

example:	$(EVAL) $(BIN)/$(MHS) Example.hs
	$(BIN)/$(MHS) -ilib Example && $(EVAL)

# does not work
exampleboot:	$(BIN)/boot$(MHS) Example.hs
	$(BIN)/boot$(MHS) -r -ilib Example && $(EVAL)

examplecomb:	$(EVAL) $(COMB)$(MHS).comb Example.hs
	$(EVAL) +RTS -r$(COMB)$(MHS).comb -RTS -r -ilib Example

clean:
	rm -rf src/*/*.hi src/*/*.o eval Main *.comb *.tmp *~ $(BIN)/* a.out $(BOOTDIR) $(OUTDIR) tmp/eval.c Tools/*.o Tools/*.hi dist-newstyle
	cd tests; make clean

###
### Make an eval.c that contains the combinator code.
###
tmp/eval.c: src/runtime/eval.c $(BIN)/eval $(COMB)$(MHS).comb 
	@mkdir -p tmp
	cp src/runtime/eval.c tmp/eval.c
	$(BIN)/eval +RTS -K10M -r$(COMB)$(MHS).comb -RTS -ilib -iTools -r Addcombs -- $(COMB)$(MHS).comb >> tmp/eval.c

###
### Make an executable that contains the combinator code.
###
$(BIN)/cmhs: tmp/eval.c
	$(GCC) -Wall -O3 tmp/eval.c -o $(BIN)/cmhs
	strip $(BIN)/cmhs

###
### Compress the binary (broken on MacOS)
###
$(BIN)/umhs: $(BIN)/cmhs
	rm -f $(BIN)/umhs
	$(UPX) -q -q -o$(BIN)/umhs $(BIN)/cmhs
###
### Test that the compiler can bootstrap
###
bootstraptest: $(EVAL)
	@mkdir -p tmp
	@echo Build stage 1 with distribution combinator file
	$(EVAL) +RTS -rcomb/mhs.comb  -RTS -ilib -isrc -otmp/mhs.comb.1 MicroHs.Main
	@echo Build stage 2 with output from stage 1
	$(EVAL) +RTS -rtmp/mhs.comb.1 -RTS -ilib -isrc -otmp/mhs.comb.2 MicroHs.Main
	cmp tmp/mhs.comb.1 tmp/mhs.comb.2 && echo Success
