#! gnumake
#
# Makefile to build libPTypes.so
#
DEPENDS = ./*.h
include ../globals.mk
#
# Local variables
#
INCLUDE		= $(ROOT_FLAGS) -I.
OBJS            = objects/ParticleInfo.o objects/ParticleTypes.o 
OBJS            += objects/PTypesDict.o
DICT_HEADERS    = ParticleInfo.h ParticleTypes.h LinkDef.h
#
# Make commands
#
all: $(OBJS) lib install
lib: libPTypes.so
clean:
	@rm -f *.o objects/*.o
	@rm -f rootdict/*.h rootdict/*.C ./PTypesDict.*
install:
	@mv -f PTypesDict.* rootdict/.
	@mv -f ./libPTypes.so ../lib/.

#
# Rule to generate the dictionary for ROOT's CINT interpreter
#
PTypesDict.C: $(DICT_HEADERS)
	@echo "Generating ptypes dictionary source file ..."
	rootcint -f $@ -c $^
	@echo "done."
#
# Rule to link shared library
#
libPTypes.so: $(OBJS)
	@echo "Linking libPTypes.so ..."
	g++ -shared $(OBJS) -o ./libPTypes.so
	@chmod 555 ./libPTypes.so
	@echo "done."