cmake_minimum_required(VERSION 3.5)
project(HybridBaryons)
set(VERSION 1.0.0)

# Add modules to find find packages
set(CMAKE_MODULE_PATH
        ${CMAKE_MODULE_PATH}
        ${PROJECT_SOURCE_DIR}/cmake)

# Add root support
list(APPEND CMAKE_PREFIX_PATH $ENV{ROOTSYS})
find_package(ROOT REQUIRED COMPONENTS RIO EG)
#include(${ROOT_USE_FILE})
set(CMAKE_CXX_FLAGS "-Ofast -Wno-return-type ${ROOT_CXX_FLAGS}")

OPTION(USE_CLIPP "Use clipp for command line options" OFF) # Off by default
IF(USE_CLIPP)
    ADD_DEFINITIONS(-DUSE_CLIPP)
ENDIF(USE_CLIPP)

ADD_DEFINITIONS(-DBOS)
ADD_DEFINITIONS(-Dsgi)


include_directories($ENV{ROOTSYS}/include/root)
include_directories(/usr/local/clas-software/build/include)
include_directories(${CMAKE_SOURCE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/get_xsect)
include_directories(${CMAKE_SOURCE_DIR}/interpol)

add_library(2pi_event_generator_lib STATIC
        anti_rot.cxx
        fermi_anti_rot.cxx
        fermi_bonn.cxx
        fermi_rot.cxx
        global.cxx
        hist_def.cxx
        hist_fill.cxx
        hist_write.cxx
        inp_file_read.cxx
        out_file_close.cxx
        out_file_fill.cxx
        out_file_open.cxx
        radcorr.cxx
        read_fit_param_files.cxx
        read_xsect_files.cxx
        rot.cxx

        get_xsect/get_xsect_14_18_lowq2_fit.cxx
        get_xsect/get_xsect_25_30.cxx
        get_xsect/get_xsect_fedotov.cxx
        get_xsect/get_xsect_gol2.cxx
        get_xsect/get_xsect_golovach.cxx
        get_xsect/get_xsect_near_threshold.cxx
        get_xsect/get_xsect_q2_130_w_gt_18_lt_21.cxx
        get_xsect/get_xsect_q2_13_wgt_3.cxx
        get_xsect/get_xsect_rip_fed_join.cxx
        get_xsect/get_xsect_ripani.cxx
        get_xsect/get_xsect_scale_gol_18_25.cxx

        interpol/interpol.cxx
        interpol/interpol_fedotov.cxx
        interpol/interpol_fedotov_thresh.cxx
        interpol/interpol_gol2.cxx
        interpol/interpol_golovach.cxx
        interpol/interpol_int.cxx
        interpol/interpol_phot_wgt_3.cxx
        interpol/interpol_q2_13_wgt_3.cxx
        interpol/interpol_rip2.cxx
        interpol/interpol_rip3.cxx
        )


add_executable(2pi_event_generator_bos ${CMAKE_SOURCE_DIR}/main_prog.cxx)
target_link_libraries(2pi_event_generator_bos 2pi_event_generator_lib ${ROOT_LIBRARIES} EG)
install(TARGETS 2pi_event_generator_bos RUNTIME DESTINATION bin)
