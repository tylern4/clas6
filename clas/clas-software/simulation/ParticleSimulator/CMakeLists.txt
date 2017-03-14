cmake_minimum_required (VERSION 2.6) 
project(ParticleSimulator)

set(CMAKE_CXX_FLAGS "-fPIC -g -Wall -Wno-write-strings")

# find ROOT
include(FindROOT.cmake)
set(INCLUDE_DIRECTORIES ${ROOT_INCLUDE_DIR})
include_directories(${INCLUDE_DIRECTORIES})
set(LINK_DIRECTORIES ${ROOT_LIBRARY_DIR})
link_directories(${LINK_DIRECTORIES})


INCLUDE_DIRECTORIES(BOSClass)
add_subdirectory(BOSClass)


set(LINK_DIR1 /group/clas/builds/64bit/STABLE/build/include)		# change this path
include_directories(${LINK_DIR1})

set(LINK_DIR2 /group/clas/builds/64bit/STABLE/build/lib)		# change this path


file(GLOB files "*.cxx")
add_library(ParticleSimulator SHARED ${files})
target_link_libraries(ParticleSimulator ${ROOT_LIBRARIES} BOSClass ${LINK_DIR2}/libbankdefs.a ${LINK_DIR2}/libbosio.a)


# to build static library, uncomment below 4 lines
#SET_TARGET_PROPERTIES(ParticleSimulator-static PROPERTIES OUTPUT_NAME "ParticleSimulator")
#SET_TARGET_PROPERTIES(ParticleSimulator-static PROPERTIES PREFIX "lib")
#add_library(ParticleSimulator-static STATIC ${files})
#target_link_libraries(ParticleSimulator-static ${ROOT_LIBRARIES} BOSClass ${LINK_DIR2}/libbankdefs.a ${LINK_DIR2}/libbosio.a)
