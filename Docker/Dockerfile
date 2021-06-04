FROM tylern4/rootv5
LABEL maintainer "tylern@jlab.org"

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

RUN mkdir -p /usr/local/cernlib
COPY cernlib /usr/local/cernlib
RUN ln -s /usr/local/root/lib/root/* /usr/local/root/lib 

RUN mkdir -p /dev

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
COPY src/clas-software /usr/local/clas-software

ENV CLASTOOL /usr/local/clas-software/analysis/ClasTool
ENV OS_NAME Linux

#scons opt=3 -j$(nproc) 2> /dev/null \
WORKDIR /usr/local/clas-software
RUN mkdir -p /usr/local/clas-software/scripts/build
RUN scons opt=3 -j$(nproc) 2> /dev/null \
    && scons install

RUN cp /usr/local/clas-software/simulation/generators/fsgen/lund_upd.dat /usr/local/clas-software/build/bin

RUN source /root/.bashrc \
    && cd /usr/local/clas-software/analysis/ClasTool \
    && make 2> /dev/null \
    && cd Utils \
    && make 2> /dev/null


ENV data_dir_2pi=/usr/local/2pi_event_generator/
COPY src/Hybrid-Baryons/2pi_event_generator /usr/local/2pi_event_generator
WORKDIR /usr/local/2pi_event_generator
COPY src/Makefile.docker .
RUN make -f Makefile.docker bos 2> /dev/null \
    && cp twopeg_bos.exe /usr/local/bin

RUN git clone https://github.com/tylern4/TWOPEG_Merger.git \
    && cd TWOPEG_Merger \
    && make \
    && cp twopeg_merge /usr/local/bin

WORKDIR /work


ENTRYPOINT ["/bin/bash"]
