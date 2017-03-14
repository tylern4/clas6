#! gnumake
# Makefile to compile the hvdiff utility
# Author: Paul Mattione (pmatt@jlab.org), 09/19/07

all: hvdiff hvdiff_coupled print2500s

exe: hvdiff hvdiff_coupled print2500s

hvdiff.o: hvdiff.cc
	@echo "Building hvdiff.o ..."
	g++ -O -Wall -g -fPIC -c -o hvdiff.o hvdiff.cc
	@echo "done."

hvdiff: hvdiff.o
	@echo "Building hvdiff ..."
	g++ -O -Wall hvdiff.o -o hvdiff
	@echo "done."

hvdiff_coupled.o: hvdiff_coupled.cc
	@echo "Building hvdiff_coupled.o ..."
	g++ -O -Wall -g -fPIC -c -o hvdiff_coupled.o hvdiff_coupled.cc
	@echo "done."

hvdiff_coupled: hvdiff_coupled.o
	@echo "Building hvdiff_coupled ..."
	g++ -O -Wall hvdiff_coupled.o -o hvdiff_coupled
	@echo "done."

print2500s.o: print2500s.cc
	@echo "Building print2500s.o ..."
	g++ -O -Wall -g -fPIC -c -o print2500s.o print2500s.cc
	@echo "done."

print2500s: print2500s.o
	@echo "Building print2500s ..."
	g++ -O -Wall print2500s.o -o print2500s
	@echo "done."

clear:
	@rm hvdiff.o
	@rm hvdiff
	@rm hvdiff_coupled.o
	@rm hvdiff_coupled
	@rm print2500s.o
	@rm print2500s
