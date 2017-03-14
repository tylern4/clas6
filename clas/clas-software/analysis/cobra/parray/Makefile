#! gnumake
#
# Makefile to build libPArray.so
#
DEPENDS         = ../ptypes/*.h ./*.h ./*.tcc
include ../globals.mk
#
# Local variables
#
INCLUDE		= $(ROOT_FLAGS) -I. -I../ptypes
OBJS            = objects/TypeInfo.o objects/PArrayDict.o
DICT_HEADERS    = PArrayElement_Base.h TypeInfo.h PArrayElement.h 
DICT_HEADERS    += ParticleArray.h LinkDef.h
#
# Make commands
#
all: $(OBJS) lib install
lib: libPArray.so
clean:
	@rm -f objects/*.o *.o
	@rm -f PArrayDict.* rootdict/*.h rootdict/*.C
install:
	@mv -f PArrayDict.* rootdict/.
	@mv -f ./libPArray.so ../lib/.
#
# Rule to generate the dictionary for ROOT's CINT interpreter
#
PArrayDict.C: $(DICT_HEADERS)
	@echo "Generating parray dictionary source file ..."
	rootcint -f $@ -c -D_generate_ROOT_dictionary_ -I../ptypes $^
	@echo "done."
#
# Rule to link shared library
#
libPArray.so: $(OBJS)
	@echo "Linking libPArray.so ..."
	g++ -shared $(OBJS) -o ./libPArray.so
	@chmod 555 ./libPArray.so
	@echo "done."