FROM tylern4/rootv5:latest

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

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

RUN ln -s $CERN_LIB $CERN \
  && ln -s /lib64/mysql/* /lib64/ \
  && ln -s /usr/include/mysql/* /usr/include/

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

RUN cd /clas_software && scons -j$(nproc)

EXPOSE 22
CMD /bin/bash
