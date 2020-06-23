FROM uofscphysics/root

COPY 2pi_event_generator /usr/local/2pi_event_generator
COPY CMakeLists.txt /usr/local/2pi_event_generator/

RUN ls -la /usr/local/2pi_event_generator/main_prog.cxx
RUN mkdir -p /usr/local/2pi_event_generator/build

WORKDIR /usr/local/2pi_event_generator/build
ENV data_dir_2pi=/data_dir_2pi/
RUN cmake .. -DUSE_CLIPP=ON && make -j4 \
    && mkdir -p /data_dir_2pi/data/ \
    && mv /usr/local/2pi_event_generator/data/* /data_dir_2pi/data/ \
    && mv 2pi_event_generator /usr/local/bin \
    && rm -rf /usr/local/2pi_event_generator

WORKDIR /work
ENTRYPOINT [ "/usr/local/bin/2pi_event_generator" ]