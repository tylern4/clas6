# Author: Mike Williams
# Makefile to build the ROOT compatible classes to store data from BOS files

ROOTFLAGS     = $(shell $(ROOTSYS)/bin/root-config --cflags)
SHAREDFLAGS   = $(CXXFLAGS) -fPIC -g $(ROOTFLAGS)
LD            = g++

OBJS          = TAGRentry.o Photons.o EventHeader.o ATrack.o Tracks.o AParticle.o Charged.o CLASdataEventDict.o CLASdata.o

all: libCLASdata.so
	@echo "Library is now up to date."

lib: all

libCLASdata.so: $(OBJS)
	@echo "Linking libCLASdata.so ..."
	@rm -f libCLASdata.so
	g++ -shared $(OBJS) -o libCLASdata.so
	@chmod 555 libCLASdata.so
	@rm -f ../libCLASdata.so
	@cp ./libCLASdata.so ../.
	@chmod 555 ../libCLASdata.so
	@rm -f $(CLAS_SLIB)/libCLASdata.so
	@cp ./libCLASdata.so $(CLAS_SLIB)/.
	@chmod 555 $(CLAS_SLIB)/libCLASdata.so
	@echo "done."

TAGRentry.o: Photons.h TAGRentry.C
	@echo "Building TAGRentry.o ..."
	g++ $(SHAREDFLAGS) -c -o TAGRentry.o TAGRentry.C
	@echo "done."

Photons.o: Photons.h Photons.C
	@echo "Building Photons.o ..."
	g++ $(SHAREDFLAGS) -c -o Photons.o Photons.C
	@echo "done."

EventHeader.o: EventHeader.h EventHeader.C
	@echo "Building EventHeader.o ..."
	g++ $(SHAREDFLAGS) -c -o EventHeader.o EventHeader.C
	@echo "done."

ATrack.o: ATrack.h ATrack.C
	@echo "Building ATrack.o ..."
	g++ $(SHAREDFLAGS) -c -o ATrack.o ATrack.C
	@echo "done."

Tracks.o: Tracks.h Tracks.C
	@echo "Building Tracks.o ..."
	g++ $(SHAREDFLAGS) -c -o Tracks.o Tracks.C
	@echo "done."

AParticle.o: AParticle.h AParticle.C
	@echo "Building AParticle.o ..."
	g++ $(SHAREDFLAGS) -c -o AParticle.o AParticle.C
	@echo "done."

Charged.o: Charged.h Charged.C
	@echo "Building Charged.o ..."
	g++ $(SHAREDFLAGS) -c -o Charged.o Charged.C
	@echo "done."

CLASdata.o: CLASdata.h CLASdata.C
	@echo "Building CLASdata.o ..."
	g++ $(SHAREDFLAGS) -c -o CLASdata.o CLASdata.C
	@echo "done."

CLASdataEventDict.C: Photons.h ATrack.h Tracks.h EventHeader.h AParticle.h Charged.h CLASdata.h  LinkDef.h
	@echo "Generating dictionary ..."
	rootcint -f $@ -c $^
	@echo "done."

CLASdataEventDict.o: CLASdataEventDict.C
	@echo "Building CLASdataEventDict.o ..."
	g++ $(SHAREDFLAGS) -c -o CLASdataEventDict.o CLASdataEventDict.C
	@echo "done."

clean:; 
	@rm -f $(OBJS)
	@rm -f libCLASdata.so
	@rm -f CLASdataEventDict.C
