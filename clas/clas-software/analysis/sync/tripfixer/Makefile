#! gnumake
# Makefile to compile the tripfixer utility
# Author: Paul Mattione (pmatt@jlab.org), 09/28/09

all: tripfixer

exe: tripfixer

tripfixer.o: tripfixer.cc
	@echo "Building tripfixer.o ..."
	g++ -O -Wall $(shell $(ROOTSYS)/bin/root-config --cflags) -g -fPIC -c -I$(ROOTSYS)/include -o tripfixer.o tripfixer.cc
	@echo "done."

#tripfixer: tripfixer.o
tripfixer: tripfixer.cc
	@echo "Building tripfixer ..."
#	g++ -O -Wall $(shell $(ROOTSYS)/bin/root-config --cflags) -I$(ROOTSYS)/include tripfixer.o -o tripfixer
	g++ -O -Wall $(shell $(ROOTSYS)/bin/root-config --cflags) $(shell $(ROOTSYS)/bin/root-config --glibs) -I$(ROOTSYS)/include tripfixer.cc -o tripfixer
	@echo "done."

clear:
	@rm tripfixer.o
	@rm tripfixer
