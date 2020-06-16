FROM tylern4/rootv5:centos
LABEL maintainer "tylern@jlab.org"

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

RUN mkdir -p /usr/local/cernlib
COPY cernlib /usr/local/cernlib
RUN ln -s /usr/local/root/lib/root/* /usr/local/root/lib 

# This is using the cernlib copied from jlab ifarm
ENV RECSIS_RUNTIME=/recsis
ENV LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/root/lib/root
ENV PATH /usr/local/clas-software/build/bin:$PATH
ENV CERN /usr/local/cernlib/x86_64_rhel6
ENV CERN_LEVEL 2005
ENV CERN_ROOT $CERN/$CERN_LEVEL
ENV CVSCOSRC $CERN/$CERN_LEVEL/src
ENV PATH $CERN/$CERN_LEVEL/src:$PATH
ENV CERN_LIB $CERN_ROOT/lib
RUN ln -s $CERN_LIB $CERN
ENV CERN_BIN $CERN_ROOT/bin
ENV LD_LIBRARY_PATH $ROOTSYS/lib
ENV CLAS_PARMS=/group/clas/parms
COPY parms /group/clas/parms

RUN mkdir -p /usr/local/cernlib
RUN mkdir -p /usr/local/clas-software
COPY bashrc /root/.bashrc
COPY env.sh /usr/local/clas-software
COPY env.csh /usr/local/clas-software
COPY clas-software /usr/local/clas-software

ENV CLASTOOL /usr/local/clas-software/analysis/ClasTool
ENV OS_NAME Linux

#RUN cd /usr/local/clas-software && scons opt=3 -j$(nproc)
RUN cd /usr/local/clas-software && scons opt=3 -j$(nproc) 2> /dev/null \
    && scons install \
    && cp /usr/local/clas-software/simulation/generators/fsgen/lund_upd.dat /usr/local/clas-software/build/bin \
    && source /root/.bashrc \
    && cd /usr/local/clas-software/analysis/ClasTool \
    && make \
    && cd Utils \
    && make


ENV data_dir_2pi=/usr/local/2pi_event_generator/
COPY Hybrid-Baryons/2pi_event_generator /usr/local/2pi_event_generator
WORKDIR /usr/local/2pi_event_generator
COPY Makefile.docker .
RUN make -f Makefile.docker bos \
    && cp twopeg_bos.exe /usr/local/bin

WORKDIR /work

ENTRYPOINT ["/bin/bash"]
