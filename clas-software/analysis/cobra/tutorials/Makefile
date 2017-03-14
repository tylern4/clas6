#! gnumake
#
# COBRASYS/configure generates globals.mk, which defines ALL_LIBS and ALL_FLAGS
# which should be all the libraries and includes/flags you'll need to compile
# COBRA software given how you've configured it. Also, for interface X, if it's
# been compiled into COBRA then Build_X_Interface should be defined in 
# globals.mk
#
include $(COBRASYS)/globals.mk

EXE        = 
ifdef Build_Bos_Interface
EXE        += BosTriggerCheck BosSkimOmega
endif

ifdef Build_CLASdata_Interface
EXE        += SkimOmega
endif

all: $(EXE)

clean:;
	@rm -f $(EXE) *.o

%: %.cxx ../lib/*.so
	g++ $(ALL_FLAGS) -c -o $*.o $*.cxx
	g++ $(ALL_FLAGS) $*.o $(ALL_LIBS) -o $*

%: %.cc ../lib/*.so
	g++ $(ALL_FLAGS) -c -o $*.o $*.cc
	g++ $(ALL_FLAGS) $*.o $(ALL_LIBS) -o $*