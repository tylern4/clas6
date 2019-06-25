BOS = bos
NOBOS = nobos
CXX           = g++ -O3 -Wno-write-strings -Wno-pragmas -I$(CLAS_BUILD)/packages/include -I$(PWD)/interpol -I$(PWD)/get_xsect -I$(PWD)
CXX_BOS       = g++ -DBOS -O3 -Wno-write-strings -Wno-pragmas -I$(CLAS_BUILD)/packages/include -I$(PWD)/interpol -I$(PWD)/get_xsect -I$(PWD)
ObjSuf        = o
ObjSuf_BOS        = o
SrcSuf        = cxx
DllSuf        = so

ROOTCFLAGS   := $(shell root-config --cflags)
ROOTLIBS     := $(shell root-config --libs)
ROOTGLIBS    := $(shell root-config --glibs)
ROOTINCLUDE  := -I$(shell root-config --incdir)

SRCS = main_prog.cxx global.cxx hist_def.cxx  inp_file_read.cxx  read_xsect_files.cxx  read_fit_param_files.cxx  out_file_open.cxx out_file_fill.cxx  out_file_close.cxx get_xsect/get_xsect_ripani.cxx get_xsect/get_xsect_fedotov.cxx get_xsect/get_xsect_q2_130_w_gt_18_lt_21.cxx  get_xsect/get_xsect_rip_fed_join.cxx get_xsect/get_xsect_near_threshold.cxx get_xsect/get_xsect_golovach.cxx get_xsect/get_xsect_14_18_lowq2_fit.cxx  get_xsect/get_xsect_scale_gol_18_25.cxx anti_rot.cxx rot.cxx interpol/interpol.cxx interpol/interpol_rip2.cxx interpol/interpol_golovach.cxx interpol/interpol_fedotov.cxx interpol/interpol_fedotov_thresh.cxx interpol/interpol_rip3.cxx interpol/interpol_q2_13_wgt_3.cxx interpol/interpol_phot_wgt_3.cxx interpol/interpol_gol2.cxx get_xsect/get_xsect_gol2.cxx get_xsect/get_xsect_25_30.cxx interpol/interpol_int.cxx radcorr.cxx fermi_bonn.cxx fermi_rot.cxx fermi_anti_rot.cxx get_xsect/get_xsect_q2_13_wgt_3.cxx

OBJS = $(SRCS:.cxx=.o)

usage:
	@echo "Usage: "
	@echo "  make bos"
	@echo "  make nobos"


bos: setcxx twopeg_$(BOS).exe
setcxx:
	rm -f *.o
	rm -f *Dict.*
	rm -f G__*
	$(eval CXX = $(CXX_BOS))
twopeg_$(BOS).exe: $(OBJS)
	$(CXX) -g -o $@ $^ -L/usr/lib64 -L/usr/local/lib -lpid -ltag -llac -lseb -lst -lclasutil -lsc -lc_bos_io -ldc -lec -lcc -ltrk -ldc -lbosio -lc_bos_io -lsc -lrecutl -lonline_dummy -lc_cern -lclasutil -lbankdefs -L$(CERN_LIB) -lpacklib -lkernlib -lnsl -lgfortran -lmathlib -lpacklib -lkernlib -lpawlib $(ROOTGLIBS)  -lEG
%.o: %.cxx
	$(CXX) -g -c $(ROOTINCLUDE) -c $<  -o $@


nobos: remove twopeg_$(NOBOS).exe
remove:
	rm -f *.o
	rm -f *Dict.*
	rm -f G__*
twopeg_$(NOBOS).exe: $(OBJS)
	$(CXX) -g -o $@ $^ -L/usr/lib64    $(ROOTGLIBS)  -lEG

clean:
	rm -f *.o
	rm -f *Dict.*
	rm -f G__*
	rm -f interpol/*.o
	rm -f get_xsect/*.o
	rm twopeg*.exe
