FROM tylern4/rootv5
LABEL maintainer "tylern@jlab.org"

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

RUN mkdir -p /usr/local/cernlib
COPY cernlib /usr/local/cernlib
RUN ln -s /usr/local/root/lib/root/* /usr/local/root/lib 

# This is using the cernlib copied from jlab ifarm
ENV RECSIS_RUNTIME=/recsis
ENV LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/root/lib/root
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

ENV TOP_DIR /usr/local/g11
ENV CLAS_PACK /usr/local/g11-software
ENV CLAS_SCRIPTS $CLAS_PACK/scripts
ENV CLAS_CMS $CLAS_PACK/cms
ENV MYSQL_LIB_PATH $MYSQLLIB
ENV MYSQLINC /usr/include/mysql

ENV OS_NAME LinuxDocker
ENV PATH $TOP_DIR/bin/$OS_NAME:$PATH
ENV CLAS_LIB /usr/local/g11/lib/$OS_NAME
ENV CLAS_CALDB_RUNINDEX calib_user.RunIndexg11a

## Build g11 software

COPY g11-software /usr/local/g11-software
COPY bashrc-g11 /root/.bashrc
COPY env-g11.sh /usr/local/g11/env.sh

WORKDIR /usr/local/g11-software

RUN cd ana \
    && make lib

RUN cd bankdefs \
    && make lib

RUN cd bos \
    && make lib

RUN cd caldb \
    && make lib

RUN cd c_bos_io \
    && make lib

RUN cd cc \
    && make lib

RUN cd c_cern \
    && make lib

RUN cd clasutil \
    && make lib

RUN cd c_sql \
    && make lib

RUN cd dc \
    && make lib

RUN cd ec \
    && make lib

RUN cd eloss \
    && make lib

RUN cd epics \
    && make lib

RUN cd fpack \
    && make lib

RUN cd fputil \
    && make lib

RUN cd gpp \
    && make lib

RUN cd gsim \
    && make lib

RUN cd Hv/src \
    && make lib

RUN cd icf \
    && make lib

RUN cd itape \
    && make lib

RUN cd lac \
    && make lib

RUN cd Map \
    && make lib

RUN cd online_dummy \
    && make lib

RUN cd patches \
    && make lib

RUN cd pid \
    && make lib

RUN cd recsis \
    && make lib

RUN cd recutl \
    && make lib

RUN cd sc \
    && make lib

RUN cd scaler \
    && make lib

RUN cd scat \
    && make lib

RUN cd seb \
    && make lib

RUN cd st \
    && make lib

RUN cd tag \
    && make lib

RUN cd trk \
    && make lib

RUN cd user \
    && make lib

RUN cd vertex \
    && make lib

RUN cd user_ana \
    && make

RUN cd gsim_bat \
    && make

RUN cd gpp \
    && make

WORKDIR /work
ENTRYPOINT ["/bin/bash"]
