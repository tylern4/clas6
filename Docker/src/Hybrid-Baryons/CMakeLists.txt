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
include(${ROOT_USE_FILE})
set(CMAKE_CXX_FLAGS "-Ofast -Wno-return-type ${ROOT_CXX_FLAGS}")

OPTION(USE_CLIPP "Use clipp for command line options" OFF) # Off by default
IF(USE_CLIPP)
    ADD_DEFINITIONS(-DUSE_CLIPP)
ENDIF(USE_CLIPP)


include_directories(${CMAKE_SOURCE_DIR}/2pi_event_generator)
include_directories(${CMAKE_SOURCE_DIR}/2pi_event_generator/get_xsect)
include_directories(${CMAKE_SOURCE_DIR}/2pi_event_generator/interpol)

add_library(2pi_event_generator_lib STATIC
        2pi_event_generator/anti_rot.cxx
        2pi_event_generator/fermi_anti_rot.cxx
        2pi_event_generator/fermi_bonn.cxx
        2pi_event_generator/fermi_rot.cxx
        2pi_event_generator/global.cxx
        2pi_event_generator/hist_def.cxx
        2pi_event_generator/hist_fill.cxx
        2pi_event_generator/hist_write.cxx
        2pi_event_generator/inp_file_read.cxx
        2pi_event_generator/out_file_close.cxx
        2pi_event_generator/out_file_fill.cxx
        2pi_event_generator/out_file_open.cxx
        2pi_event_generator/radcorr.cxx
        2pi_event_generator/read_fit_param_files.cxx
        2pi_event_generator/read_xsect_files.cxx
        2pi_event_generator/rot.cxx

        2pi_event_generator/get_xsect/get_xsect_14_18_lowq2_fit.cxx
        2pi_event_generator/get_xsect/get_xsect_25_30.cxx
        2pi_event_generator/get_xsect/get_xsect_fedotov.cxx
        2pi_event_generator/get_xsect/get_xsect_gol2.cxx
        2pi_event_generator/get_xsect/get_xsect_golovach.cxx
        2pi_event_generator/get_xsect/get_xsect_near_threshold.cxx
        2pi_event_generator/get_xsect/get_xsect_q2_130_w_gt_18_lt_21.cxx
        2pi_event_generator/get_xsect/get_xsect_q2_13_wgt_3.cxx
        2pi_event_generator/get_xsect/get_xsect_rip_fed_join.cxx
        2pi_event_generator/get_xsect/get_xsect_ripani.cxx
        2pi_event_generator/get_xsect/get_xsect_scale_gol_18_25.cxx

        2pi_event_generator/interpol/interpol.cxx
        2pi_event_generator/interpol/interpol_fedotov.cxx
        2pi_event_generator/interpol/interpol_fedotov_thresh.cxx
        2pi_event_generator/interpol/interpol_gol2.cxx
        2pi_event_generator/interpol/interpol_golovach.cxx
        2pi_event_generator/interpol/interpol_int.cxx
        2pi_event_generator/interpol/interpol_phot_wgt_3.cxx
        2pi_event_generator/interpol/interpol_q2_13_wgt_3.cxx
        2pi_event_generator/interpol/interpol_rip2.cxx
        2pi_event_generator/interpol/interpol_rip3.cxx
        )


add_executable(2pi_event_generator ${CMAKE_SOURCE_DIR}/2pi_event_generator/main_prog.cxx)
target_link_libraries(2pi_event_generator 2pi_event_generator_lib ${ROOT_LIBRARIES})
install(TARGETS 2pi_event_generator RUNTIME DESTINATION bin)
