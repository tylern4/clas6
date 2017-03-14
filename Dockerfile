FROM centos:7
ENV TERM xterm

RUN export CORES=$(nproc)

ENV ROOTSYS /usr/local/root
ENV PATH $ROOTSYS/bin:$PATH
ENV PYTHONDIR $PYTHONDIR:$ROOTSYS
ENV LD_LIBRARY_PATH $ROOTSYS/lib:$PYTHONDIR/lib:$ROOTSYS/bindings/pyroot:$LD_LIBRARY_PATH
ENV DYLD_LIBRARY_PATH /usr/lib:$ROOTSYS/lib:$PYTHONDIR/lib:$ROOTSYS/bindings/pyroot:$DYLD_LIBRARY_PATH
ENV PYTHONPATH $PYTHONPATH:$ROOTSYS/lib:$ROOTSYS/bindings/pyroot

RUN yum -y install epel-release && yum -y update \
    && yum install -y perl git cmake gcc-c++ gcc binutils bash \
    make libX11-devel libXpm-devel libXft-devel libXext-devel \
    subversion scons patch expat-devel mysql-devel bzip2-devel \
    blas-devel blas-static lapack-devel lapack-static bzip2 tcsh \
    sqlite-devel gcc-gfortran openssl-devel pcre-devel \
    mesa-libGL-devel mesa-libGLU-devel glew-devel ftgl-devel \
    fftw-devel cfitsio-devel graphviz-devel \
    avahi-compat-libdns_sd-devel libldap-dev python-devel \
    libxml2-devel gsl-static xz tar file scons openmotif-devel \
    wget libXmu-devel libXp-devel libXt-devel libjpeg-devel libpng-devel \
    tcl tcl-devel tk tk-devel imake \
    && yum clean all \
    && ln -s /usr/lib64/liblapack.a /usr/lib64/liblapack3.a

# https://github.com/root-project/root/archive/v5-34-36.tar.gz
COPY v5-34-36.tar.gz /cern/

RUN cd /cern/ \
	&& tar -xvf v5-34-36.tar.gz \
	&& cd root-5-34-36/ \
	&& ./configure --all --prefix=$ROOTSYS \
	&& make -j2 && make && make install \
	&& source bin/thisroot.sh



#	https://downloads.sourceforge.net/project/scons/scons/2.5.1/scons-2.5.1.tar.gz
#COPY scons-2.5.1.tar.gz /tmp

# https://downloads.sourceforge.net/project/scons/scons/2.5.1/scons-2.5.1.tar.gz
#RUN cd /tmp \
	#&& tar -xvf scons-2.5.1.tar.gz \
	#&& cd scons-2.5.1 \
	#&& python setup.py install

RUN mkdir -p /usr/local/cernlib

COPY cernlib /usr/local/cernlib
# http://cernlib.web.cern.ch/cernlib/download/2006_source/tar/2006_src.tar.gz
# http://cernlib.web.cern.ch/cernlib/download/2006_source/tar/include.tar.gz
# COPY 2006_src.tar.gz /usr/local/cernlib
# COPY include.tar.gz /usr/local/cernlib

#RUN cd /usr/local/cernlib \
#  && tar -xvf 2006_src.tar.gz \
#  && tar -xvf include.tar.gz

#ENV CERN=/usr/local/cernlib
# This is using the cernlib copied from jlab ifarm
ENV CERN /usr/local/cernlib/x86_64_rhel7
#ENV CERN_LEVEL=2006
ENV CERN_LEVEL 2005
ENV CERN_ROOT $CERN/$CERN_LEVEL
ENV CVSCOSRC $CERN/$CERN_LEVEL/src
ENV PATH $CERN/$CERN_LEVEL/src:$PATH
ENV CERN_LIB $CERN_ROOT/lib
ENV CERN_BIN $CERN_ROOT/bin
ENV LD_LIBRARY_PATH $ROOTSYS/lib

RUN cp -r $CERN_LIB $CERN
#RUN cd $CERN_ROOT \
#  && mkdir -p build bin lib build/log \
#  && cd $CERN_ROOT/build \
#  && $CVSCOSRC/config/imake_boot \
#  && make MAKE=make bin/kuipc \
#  && make MAKE=make scripts/Makefile \
#  && cd scripts \
#  && make MAKE=make install.bin \
#  && cd $CERN_ROOT/build \
#  && make MAKE=make
COPY clas-software /clas_software

RUN cd /clas_software && scons -j4


EXPOSE 22
CMD /bin/bash
