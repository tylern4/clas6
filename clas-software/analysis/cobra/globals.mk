#! gnumake 
###############################################################################
#
# This Makefile contains a list of all global variables used by the various 
# other Makefiles in this package. 
#
# Note: This file is generated automatically by configure. 
#
# Note: Make sure COBRASYS enviornment variable is properly set 
#
###############################################################################
#
# ROOT flags and libs 
#
ROOT_FLAGS	= $(shell $(ROOTSYS)/bin/root-config --cflags)
ROOT_LIBS	= $(shell $(ROOTSYS)/bin/root-config --libs) 
#
# CLAS includes and libs 
#
CLAS_FLAGS      = $(shell $(COBRASYS)/bin/clas-config --cflags) 
CLAS_LIBS       = $(shell $(COBRASYS)/bin/clas-config --libs) 
CLAS_LIBS_MIN   = $(shell $(COBRASYS)/bin/clas-config --libs_min) 
CLAS_SLIBS      = $(shell $(COBRASYS)/bin/clas-config --slibs) 
#
# COBRA includes and libs 
#
COBRA_FLAGS     = $(shell $(COBRASYS)/bin/cobra-config --cflags) 
COBRA_LIBS      = $(shell $(COBRASYS)/bin/cobra-config --libs) 
#
# FORTRAN to C lib (default is g2c, -f2c option in configure) 
#
F2C_LIB = -lg2c 
#
# CERN libs 
#
CERN_LIBS = -L$(CERN_ROOT)/lib -lpacklib -lnsl 
#
# Compiler Flags 
#
CXX_FLAGS       = -O2 -Wall -fPIC 
#
# General pattern to build objects from source files 
#
objects/%.o: %.C $(DEPENDS)
	g++ $(FLAGS) $(INCLUDE) -c -o objects/$*.o $*.C 
#
# ClasEvent Interfaces defined (--interface option in configure)) 
#
Build_Bos_Interface = 1 
Build_CLASdata_Interface = 1 
#
# Packages to build in (--build option in configure) 
#
Build_g11pcor = 1 
Build_g10pcor = 1 
Build_g1c2445pcor = 1 
Build_g6cpcor = 1 
#
# Preproccessor flags for builds (along with libs) 
#
BUILD_FLAGS     =  -D_compile_g11pcor -D_compile_g10pcor -D_compile_g1c2445pcor -D_compile_g6cpcor 
BUILD_LIBS      =  -lg11pcor -lg10pcor -lg1c2445pcor -lg6cpcor 
#
# Overall LIBS and FLAGS 
#
ALL_FLAGS = $(CXX_FLAGS) $(BUILD_FLAGS) $(CLAS_FLAGS) $(COBRA_FLAGS) $(ROOT_FLAGS) 
ALL_LIBS  =  $(COBRA_LIBS) $(CLAS_SLIBS) $(CLAS_LIBS) $(BUILD_LIBS) $(CERN_LIBS) $(F2C_LIB) $(ROOT_LIBS) 
