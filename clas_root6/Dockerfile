FROM tylern4/docker-root:rootv6

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

RUN mkdir -p /usr/local/cernlib
COPY cernlib /usr/local/cernlib

# This is using the cernlib copied from jlab ifarm
ENV CERN /usr/local/cernlib/x86_64_rhel7
ENV CERN_LEVEL 2005
ENV CERN_ROOT $CERN/$CERN_LEVEL
ENV CVSCOSRC $CERN/$CERN_LEVEL/src
ENV PATH $CERN/$CERN_LEVEL/src:$PATH
ENV CERN_LIB $CERN_ROOT/lib
ENV CERN_BIN $CERN_ROOT/bin
ENV LD_LIBRARY_PATH $ROOTSYS/lib
RUN ln -s $CERN_LIB $CERN

COPY clas-software /clas_software

RUN cd /clas_software && scons -j$(nproc)

ENV PATH /clas_software/build/bin:$PATH

EXPOSE 22
ENTRYPOINT cd $ROOTSYS && source bin/thisroot.sh && cd && /bin/bash
