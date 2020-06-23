FROM tylern4/rootv5
LABEL maintainer "tylern@jlab.org"

ENV MYSQLINC /usr/include/mysql
ENV MYSQLLIB /usr/lib64/mysql

RUN mkdir -p /usr/local/cernlib
#COPY cernlib /usr/local/cernlib
RUN ln -s /usr/local/root/lib/root/* /usr/local/root/lib 

ENV CERN /usr/local/cernlib/x86_64_rhel6
ENV CERN_LEVEL 2005
ENV CERN_ROOT $CERN/$CERN_LEVEL
ENV CVSCOSRC $CERN/$CERN_LEVEL/src
ENV PATH $CERN/$CERN_LEVEL/src:$PATH
ENV CERN_LIB $CERN_ROOT/lib
ENV CERN_BIN $CERN_ROOT/bin

WORKDIR /usr/local/cernlib/x86_64_rhel6
RUN wget --quiet http://www-zeuthen.desy.de/linear_collider/cernlib/new/cernlib-2005-all-new.tgz
RUN wget --quiet http://www-zeuthen.desy.de/linear_collider/cernlib/new/cernlib.2005.install.2019.01.21.tgz
RUN wget --quiet http://www-zeuthen.desy.de/linear_collider/cernlib/new/cernlib.2005.corr.2019.01.21.tgz
RUN tar -xvf cernlib-2005-all-new.tgz \
	&& tar -xvf cernlib.2005.corr.2019.01.21.tgz \
	&& tar -xvf cernlib.2005.install.2019.01.21.tgz

RUN ./Install_cernlib_and_lapack
RUN cp -r /usr/local/cernlib/x86_64_rhel6/2005/lib /usr/local/cernlib/x86_64_rhel6
RUN ls /usr/local/cernlib/x86_64_rhel6/lib/*

# This is using the cernlib copied from jlab ifarm
ENV RECSIS_RUNTIME=/recsis
ENV LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/root/lib/root
ENV PATH /usr/local/clas-software/build/bin:$PATH
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

#RUN cd /usr/local/clas-software && scons opt=3 -j$(nproc) 2> /dev/null
RUN cd /usr/local/clas-software && scons opt=3 -j$(nproc) \
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
