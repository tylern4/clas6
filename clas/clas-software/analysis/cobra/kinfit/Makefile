#! gnumake
#
# Makefile to build object files from kinfit source files
#
DEPENDS = ../clasruns/*.h
include ../globals.mk
#
# Local variables
#
INCLUDE		= $(CLAS_FLAGS) $(COBRA_FLAGS) $(ROOT_FLAGS) -I.
OBJS            = objects/KFitUtils.o objects/KinFit.o
#
# Make commands
#
all: $(OBJS) lib install
lib: libKinFit.so
clean:
	@rm -f objects/*.o *.o
install:
	@mv -f ./libKinFit.so ../lib/.

#
# Rule to link shared library
#
libKinFit.so: $(OBJS)
	@echo "Linking libKinFit.so ..."
	g++ -shared $(OBJS) -o ./libKinFit.so
	@chmod 555 ./libKinFit.so
	@echo "done."