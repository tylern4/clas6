FROM tylern4/rootv5:centos
LABEL maintainer "tylern@jlab.org"

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

RUN mkdir -p /usr/local/cernlib
COPY cernlib /usr/local/cernlib

# This is using the cernlib copied from jlab ifarm
ENV CERN /usr/local/cernlib/x86_64_rhel6
ENV CERN_LEVEL 2005
ENV CERN_ROOT $CERN/$CERN_LEVEL
ENV CVSCOSRC $CERN/$CERN_LEVEL/src
ENV PATH $CERN/$CERN_LEVEL/src:$PATH
ENV CERN_LIB $CERN_ROOT/lib
RUN ln -s $CERN_LIB $CERN
ENV CERN_BIN $CERN_ROOT/bin
ENV LD_LIBRARY_PATH $ROOTSYS/lib
ENV CLAS_PARMS /clas/parms
COPY parms /clas/parms

RUN mkdir -p /usr/local/cernlib
RUN mkdir -p /usr/local/clas-software
COPY bashrc /root/.bashrc
COPY env.sh /usr/local/clas-software
COPY env.csh /usr/local/clas-software
COPY clas-software /usr/local/clas-software

ENV CLASTOOL /usr/local/clas-software/analysis/ClasTool
ENV OS_NAME Linux

RUN cd /usr/local/clas-software && scons opt=3 -j$(nproc) 2> /dev/null \
    && scons install \
    && source /root/.bashrc \
    && cd /usr/local/clas-software/analysis/ClasTool \
    && make \
    && cd Utils \
    && make

COPY Hybrid-Baryons /usr/local/Hybrid-Baryons
ENV data_dir_2pi /usr/local/
RUN cd /usr/local/Hybrid-Baryons/2pi_event_generator \
    && make bos \
    && make nobos \
    && cp twopeg_bos.exe /usr/local/bin/twopeg_bos \
    && cp twopeg_nobos.exe /usr/local/bin/twopeg_lund \
    && cp -r data /usr/local/data


ENV PATH /usr/local/clas-software/build/bin:$PATH

WORKDIR /root/code

ENTRYPOINT ["/bin/bash"]
