#
#
ROOTCFLAGS    = $(shell root-config --cflags)
ROOTLIBS      = $(shell root-config --libs)
ROOTGLIBS     = $(shell root-config --glibs)
#
INCLUDES = $(MYSQL_INCLUDE_PATH)
#==========================================================
OBJECTS = TDBItem.o TDBItemDict.o TQuery.o TQueryDict.o TMapUtils.o TMapUtilsDict.o TRunIndex.o TRunIndexDict.o

all: $(OBJECTS)
	ar rv libMapUtils.a $(OBJECTS)

#==========================================================
clean:
	@echo "Cleaning object and backup files...."
	@rm -rf *.o *~
	@echo "Done..."
#
%.o:%.cc
	@echo Compiling File $<
	$(CXX) $(CXXFLAGS) -c $< $(ROOTCFLAGS) -I. -I../Physics -I$(INCLUDES)
%.o:%.cxx
	@echo Compiling File $<
	$(CXX) $(CXXFLAGS) -c $< $(ROOTCFLAGS) -I. -I../Physics

T%Dict.cc: T%.h
	$(ROOTSYS)/bin/rootcint -f $@ -c -I$(INCLUDES) $< $(<:.h=LinkDef.h)
