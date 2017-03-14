#! gnumake
#
# Makefile to build object files from clasruns source files
#
DEPENDS         = ./*.h
include ../globals.mk
#
# Local variables
#
INCLUDE		= $(ROOT_FLAGS) -I.
OBJS            = objects/ClasRuns.o objects/ClasRunsDict.o
DICT_HEADERS    = ClasRunPeriod.h ClasRuns.h LinkDef.h
#
# Make commands
#
all: $(OBJS) lib install
lib: libClasRuns.so
clean:
	@rm -f *.o objects/*.o
	@rm -f rootdict/*.h rootdict/*.C ./ClasRunsDict.*
install:
	@mv -f ClasRunsDict.* rootdict/.
	@mv -f ./libClasRuns.so ../lib/.

#
# Rule to generate the dictionary for ROOT's CINT interpreter
#
ClasRunsDict.C: $(DICT_HEADERS)
	@echo "Generating clasruns dictionary source file ..."
	rootcint -f $@ -c $^
	@echo "done."
#
# Rule to link shared library
#
libClasRuns.so: $(OBJS)
	@echo "Linking libClasRuns.so ..."
	g++ -shared $(OBJS) -o ./libClasRuns.so
	@chmod 555 ./libClasRuns.so
	@echo "done."