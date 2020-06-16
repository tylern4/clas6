# Sources, executable and enviroment
# SUFFIXES:
# C++      .C  .cpp
# C        .c  .cc
# Fortran  .f


PROGRAMS     := get_tbla



# Where do you want it?
Obj_dir      := obj
Bin_dir      := .

# Turn on stuff you need with YES or NO
MY_FLAG   := NO
CLAS_FLAG := YES
CERN_FLAG := YES
ROOT_FLAG := YES
QT_FLAG   := NO
OPT       := NO
STATIC    := NO
DEBUG     := NO
SQL_MY    := YES
DB        := YES
FORTRAN   := YES
DUALMP    := NO


# ------------------ miscellaneous -------------------------------------------------------------------------------------
standard_Libs := -L/usr/lib  -lm -lc -lstdc++ -lz -ltcl -lg2c
CppFLAGS      := -fPIC

ifeq ($(DEBUG),YES)
 CppFLAGS   += -g
 #ADD_DEBUG  += _debug
 
endif

ifeq ($(STATIC),YES)
 CppFLAGS   += -static
endif

ifeq ($(OPT),YES)
# Aligning "double" variables on a will produce code that runs somewhat faster on a Pentium at the expense of more memory.
# -malign-double
 CppFLAGS   += -O2
 ifeq ($(DUALMP),YES)
  CppFLAGS  += -march=athlon-mp -mfpmath=sse -m32
 endif
endif


# ------------------ clas stuff ----------------------------------------------------------------------------------------
ifeq ($(CLAS_FLAG),YES)
 MAP := mapmanager
 ifeq ($(DB),YES)
  MAP := caldbMap
 endif



 inc_CLAS := -I$(CLAS_PACK)/include -I$(CLAS_PACK)/inc_derived
 clasLibs := -L$(CLAS_LIB)  -lpid$(ADD_DEBUG)      -lsc$(ADD_DEBUG)     -ltag$(ADD_DEBUG)      -lcc$(ADD_DEBUG)         \
			    -lst$(ADD_DEBUG)       -lec$(ADD_DEBUG)       -llac$(ADD_DEBUG)    -lclasutil$(ADD_DEBUG)   \
			    -lc_bos_io$(ADD_DEBUG) -lfputil$(ADD_DEBUG)   -lrecutl$(ADD_DEBUG) -lbos$(ADD_DEBUG)        \
			    -lfpack$(ADD_DEBUG)    -lvertex$(ADD_DEBUG)   -ltrk$(ADD_DEBUG)    -lpid$(ADD_DEBUG)        \
			    -lsc$(ADD_DEBUG)       -lc_bos_io$(ADD_DEBUG) -l$(MAP)$(ADD_DEBUG) -lclasutil$(ADD_DEBUG)   \
			    -lbankdefs$(ADD_DEBUG) -lc_cern$(ADD_DEBUG)   -ldc$(ADD_DEBUG)     -lrecutl$(ADD_DEBUG)     \
			    -lst$(ADD_DEBUG)       -l$(MAP)$(ADD_DEBUG)
endif


# ------------------ cern stuff ----------------------------------------------------------------------------------------
# nsl (network service lybrary) added for cernlib 2000 and up
ifeq ($(CERN_FLAG),YES)
 cernLibs:= -L$(CERN_LIB) -lpacklib -lmathlib -lkernlib -lpawlib  -lkernlib -lmathlib -lnsl
endif


# ------------------ root stuff ----------------------------------------------------------------------------------------
ifeq ($(ROOT_FLAG),YES)
 inc_ROOT := -I$(ROOTSYS)/include
 rootLibs := $(shell root-config --libs) -lMinuit
endif



# ------------------ mysql stuff ---------------------------------------------------------------------------------------
ifeq ($(SQL_MY),YES)
 mysqlLibs := -L$(MYSQLIB) -lmysqlclient
 mysqlIncs := -I$(MYSQINC) -I$(CLAS_PACK)/caldb/Map
endif



# ------------------ my software ---------------------------------------------------------------------------------------
ifeq ($(MY_FLAG),YES)
  inc_MY := -I$(MU)/include
  lib_MY := -L$(TOP_DIR)/lib/Linux
endif


# ------------------ Qt software ---------------------------------------------------------------------------------------
ifeq ($(QT_FLAG),YES)
 inc_QT := -I$(QTDIR)/include -I. -I./utils -I./gui
 lib_QT := -L$(QTDIR)/lib -L/usr/X11R6/lib -lqt -lGLU -lGL -lXmu -lICE -lSM -ldl -lXext -lX11 -lm -lGL -lXinerama -lXrender  -lfreetype
endif

# ------------------ compilers -----------------------------------------------------------------------------------------
CC  := gcc
FC  := g77
Cpp := g++
MOC := moc

# ----------- Include Files and Libraries ------------------------------------------------------------------------------
includes := $(inc_ROOT) $(inc_CLAS) $(inc_MY) $(inc_QT)  $(mysqlIncs)
All_Libs := $(lib_MY) $(lib_QT) $(clasLibs) $(cernLibs) $(rootLibs) $(FLibs) $(mysqlLibs) $(standard_Libs)


# Objects and binaries directory, object files


Cpp_Sources :=    $(wildcard $(PROGRAMS).C )
F77_Sources :=    $(wildcard $(PROGRAMS).f )


Objs        :=    $(patsubst %.C,   $(Obj_dir)/%.oo, $(Cpp_Sources) ) \
                  $(patsubst %.f,   $(Obj_dir)/%.o,  $(F77_Sources)   )




$(PROGRAMS) :     $(Objs)
	          @echo
	          @echo "Initiating linking --> " $@
	          @$(Cpp) $^ -o $(Bin_dir)/$@ $(CppFLAGS) $(All_Libs)
	          @echo Linking succesful !
	          @echo
	          @echo Ready to execute .
	          @echo



$(Obj_dir)/%.oo : %.C
	          @echo
	          @echo Compiling $^ ...
	          @-mkdir -p $(Obj_dir)
	          @$(Cpp) -c $^ -o $@ $(CppFLAGS) $(includes)
	          @echo Compilation of $^ succesful : $@ made .
	          @echo

$(Obj_dir)/%.o :  %.f
	          @echo
	          @echo Compiling $^ ...
	          @-mkdir -p $(Obj_dir)
	          @$(FC) -c $^ -o $@ $(FFLAGS) $(includes)
	          @echo Compilation of $^ succesful : $@ made .
	          @echo



clean: ;
		 @echo
		 @echo
		 @echo Cleaning...
		 rm -f $(Objs) $(Moc) $(EXE)
		 @echo
		 @echo
env: show_lib show_inc show_misc


show_lib:
	@echo
	@echo
	@echo
	@echo "            "Libraries for $(OSNAME):
	@echo
	@echo Standard Libs "           ": $(standard_Libs)
	@echo Cern Libs "               ": $(CERN_FLAG) $(cernLibs)
	@echo Clas Libs "               ": $(CLAS_FLAG) $(clasLibs)
	@echo ROOT Libs "               ": $(ROOT_FLAG) $(rootLibs)
	@echo Fortran Libs "            ": $(FORT_FLAG) $(FLibs)
	@echo MYSQL Libs "              ": $(SQL_MY)    $(mysqlLibs)
	@echo Libraries used"           ": $(All_Libs)
	@echo
	@echo


show_inc:
	@echo
	@echo
	@echo
	@echo "            "Includes for $(OSNAME):
	@echo
	@echo Clas Includes "           ": $(CLAS_FLAG) $(inc_CLAS)
	@echo ROOT Includes "           ": $(ROOT_FLAG) $(inc_ROOT)
	@echo MY Software"              ": $(MY_FLAG) $(inc_MY)
	@echo Include Files"            ": $(includes)
	@echo
	@echo


show_misc:
	@echo
	@echo
	@echo
	@echo "            "Miscellaneous for $(OSNAME):
	@echo
	@echo C++ Compilator"           ":  $(Cpp)
	@echo C Compilator"             ":  $(CC)
	@echo Fortran77 Compilator"	 ": $(FC)
	@echo
	@echo
	@echo Executable:"              ":  $(PROGRAMS)
	@echo C++ Sources"              ":  $(Cpp_Sources)
	@echo F77 Sources"              ":  $(F77_Sources)
	@echo Objects"                  ":  $(Objs)
	@echo
	@echo
	@echo Object Directory"         ":  $(Obj_dir)
	@echo Bin Files Directory"      ":  $(Bin_dir)
	@echo
	@echo Optimization:"            ":  $(OPT)
	@echo
	@echo Flags:"                   ":  $(CppFLAGS)
	@echo
	@echo
	@echo

