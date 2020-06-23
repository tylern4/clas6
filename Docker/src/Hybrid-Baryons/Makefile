# Makefile to be used when integrated into https://github.com/JeffersonLab/clas12-mcgen

all:
	rm -rf build;
	mkdir build; 
	cd build && cmake .. -DUSE_CLIPP=ON -DCMAKE_INSTALL_PREFIX=${JLAB_SOFTWARE}/clas12 && make install;