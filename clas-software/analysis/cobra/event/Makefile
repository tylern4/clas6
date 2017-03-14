#! gnumake
#
# Makefile to build object files from event source files
#
DEPENDS  = ../ptypes/*.h ../parray/*.h ../parray/*.tcc ./*.h
include ../globals.mk
#
# Local variables
#
INCLUDE		= $(ROOT_FLAGS) -I. -I../ptypes -I../parray
OBJS            = objects/Photon.o objects/Particle.o objects/Event.o
OBJS            += objects/EventDict.o
DICT_HDRS       = Photon.h Particle.h Event.h LinkDef.h
#
# Make commands
#
all: $(OBJS) lib install
lib: libEvent.so
clean:
	@rm -f objects/*.o *.o
	@rm -f ./rootdict/*.h rootdict/*.C EventDict.*
install:
	@mv -f EventDict.* rootdict/.
	@mv -f libEvent.so ../lib/.
#
# Rule to generate the dictionary for ROOT's CINT interpreter
#
EventDict.C: $(DICT_HDRS)
	@echo "Generating event dictionary source file ..."
	rootcint -f $@ -c -D_generate_ROOT_dictionary_ -I../ptypes -I../parray $^
	@echo "done."
#
# Rule to link shared library
#
libEvent.so: $(OBJS)
	@echo "Linking libEvent.so ..."
	g++ -shared $(OBJS) -o ./libEvent.so
	@chmod 555 ./libEvent.so
	@echo "done."