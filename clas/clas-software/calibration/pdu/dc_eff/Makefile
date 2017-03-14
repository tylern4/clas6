# Sources, executable and enviroment
# For C++ suffix .C or .cc is mandatory
# Just for linux for now sorry folks

#Cpp_Sources =  dc_eff.C
Cpp_Sources =  groups.C

#EXE = dc_eff
EXE = groups

Obj_dir := obj
Bin_dir := .

# Turn on stuff you need with YES or NO

MY_FLAG   = NO
CLAS_FLAG = YES
CERN_FLAG = NO
ROOT_FLAG = YES
OPT       = NO
STATIC    = NO
DEBUG     = NO
SQL_MY    = YES
DB        = YES
FORTRAN   = NO
DUALMP    = NO


# ------------------ miscellaneous -------------------------------------------------------------------------------------
standard_Libs := -L/usr/lib  -lm -lc -lstdc++ -lz -ltcl -lg2c
CppFLAGS      := -fPIC
CFLAGS        :=
FFLAGS        :=

ifeq ($(DEBUG),YES)
 CppFLAGS   += -g
 ADD_DEBUG  += _debug
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


# ------------------ compilers -----------------------------------------------------------------------------------------
Cpp := g++

# ----------- Include Files and Libraries ------------------------------------------------------------------------------
includes := $(inc_ROOT) $(inc_CLAS) $(inc_MY) $(inc_QT)  $(mysqlIncs)
All_Libs := $(lib_MY) $(lib_QT) $(clasLibs) $(cernLibs) $(rootLibs) $(FLibs) $(mysqlLibs) $(standard_Libs)

# Objects and binaries directory, object files

Objs    := $(patsubst %.C,   $(Obj_dir)/%.o, $(Cpp_Sources) ) \


$(EXE) :         $(Objs)
	         @echo
	         @echo "Initiating linking --> " $@
	         @$(Cpp) $^ -o $(Bin_dir)/$@ $(CppFLAGS) $(All_Libs)
	         @echo Linking succesful !
	         @echo
	         @echo Ready to execute .
	         @echo

$(Obj_dir)/%.o : %.C
	         @-mkdir -p $(Obj_dir)
	         @echo
	         @echo Compiling $^ ...
	         @$(Cpp) -c $^ -o $@ $(CppFLAGS) $(includes)
	         @echo Compilation of $^ succesful : $@ made .
	         @echo


# ------------------ environment --------------------------------------------------------------------------------------
env:
	@clear    
	@echo
	@echo "            "Enviroment for $(OSNAME):
	@echo
	@echo C++ Compilator"           ": $(Cpp)
	@echo C Compilator"             ": $(CC)
	@echo Fortran77 Compilator"	": $(FC)
	@echo
	@echo Clas Includes "           ": $(CLAS_FLAG) $(inc_CLAS)
	@echo ROOT Includes "           ": $(ROOT_FLAG) $(inc_ROOT)
	@echo MY Software"              ": $(MY_FLAG) $(inc_MY)
	@echo Include Files"            ": $(includes)
	@echo
	@echo Standard Libs "           ": $(standard_Libs)
	@echo Cern Libs "               ": $(CERN_FLAG) $(cernLibs)
	@echo Clas Libs "               ": $(CLAS_FLAG) $(clasLibs)
	@echo ROOT Libs "               ": $(ROOT_FLAG) $(rootLibs)
	@echo Fortran Libs "            ": $(FORT_FLAG) $(FLibs)
	@echo MYSQL Libs "              ": $(SQL_MY)    $(mysqlLibs)
	@echo Libraries used"           ": $(All_Libs)
	@echo
	@echo Object Directory"         ": $(Obj_dir)
	@echo Bin Files Directory"      ": $(Bin_dir)
	@echo
	@echo Optimization:"            ": $(OPT)
	@echo
	@echo Flags:"                   ": $(CFLAGS) $(CppFLAGS)
	@echo
	@echo



# ------------------ cleaning... ---------------------------------------------------------------------------------------
clean: ;     
	@clear    
	@echo
	@echo
	@echo Cleaning...
	rm -f $(Objs) $(Moc) $(EXE)

