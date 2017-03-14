#! gnumake
#
# Makefile to build object files from CLASevent/event source files
#
DEPENDS = ../parray/*.h ../parray/*.tcc ../event/*.h  ../kinfit/*.h 
DEPENDS += ../ptypes/*.h ./*.h ../clasruns/*.h
FLAGS   = $(CXX_FLAGS) $(BUILD_FLAGS)
include ../globals.mk
#
# Local variables
#
INCLUDE		= $(ROOT_FLAGS) $(CLAS_FLAGS) -I../event -I. -I../include
OBJS            = objects/Index.o objects/ClasEvent.o
ifdef Build_CLASdata_Interface
OBJS            += objects/CLASdata_Interface.o
endif
ifdef Build_Bos_Interface
OBJS            := objects/Bos.o $(OBJS) objects/BOS_Interface.o
endif
#
# Make commands
#
all: $(OBJS) lib install
lib: libClasEvent.so
clean:
	@rm -f objects/*.o
install:
	@mv -f libClasEvent.so ../lib/.
#
# Rule to link shared library
#
libClasEvent.so: $(OBJS)
	@echo "Linking libClasEvent.so ..."
	g++ -shared $(OBJS) -o ./libClasEvent.so
	@chmod 555 ./libClasEvent.so
	@echo "done."