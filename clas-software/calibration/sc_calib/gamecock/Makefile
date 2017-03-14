CXX           = g++
ObjSuf        = o
SrcSuf        = cxx
ExeSuf        =
DllSuf        = so

ROOTCFLAGS   := $(shell $(ROOTSYS)/bin/root-config --cflags)
ROOTLIBS     := $(shell $(ROOTSYS)/bin/root-config --libs)
ROOTGLIBS    := $(shell $(ROOTSYS)/bin/root-config --glibs)
ROOTINCLUDE  := $(shell $(ROOTSYS)/bin/root-config --cflags)

CLAS_INCLUDES += $(ROOTINCLUDE)
include $(CLAS_CMS)/Makefile

%.o: %.cxx
	$(CXX) -g -c -Wall -DGAMECOCK_RESOURCE=\"$(shell pwd)\" $(ROOTINCLUDE) -c $<
#	$(CXX) -g -c  $(ROOTINCLUDE) -c $<



exe: $(EXE)

$(EXE): gamecock.o Calibration.o JndexBox.o JInteractive.o JSurvey.o \
    SConstants.o ShowSector.o fitlinear.o fittimewalk.o \
    fitlandau.o fitgauss.o fittdc.o showsec.o Regression.o \
    SingleStripe.o ButtonOkCancel.o ParameterSlider.o SliderBox.o \
    CheckinBox.o CheckinValues.o  MySQL.o CaldbServer.o	NeedInput.o \
    ReadRoc.o SlotAverage.o EnterValues.o CaldbBox.o SectorAxis.o \
    ReadPipe.o Inx.o BankDescription.o
	$(CXX) -g -o $@ $^ $(ROOTLIBS) $(ROOTGLIBS)
