cmake_minimum_required (VERSION 2.6)
project(myProject)


set(CMAKE_CXX_FLAGS "-g -Wall")


include(FindROOT.cmake)
set(INCLUDE_DIRECTORIES ${ROOT_INCLUDE_DIR})
include_directories(${INCLUDE_DIRECTORIES})
set(LINK_DIRECTORIES ${ROOT_LIBRARY_DIR})
link_directories(${LINK_DIRECTORIES})


set(LINK_DIR1 /u/home/cnepali/Software12/ParticleSimulator)		# change this path
include_directories(${LINK_DIR1})


set(LINK_DIR2 ${LINK_DIR1}/BOSClass)
include_directories(${LINK_DIR2})


set(LINK_DIR3 /group/clas/builds/64bit/STABLE/build/include)			# change this path
include_directories(${LINK_DIR3})



ADD_EXECUTABLE(test.exe "test.cxx")
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	TARGET_LINK_LIBRARIES(test.exe ${ROOT_LIBRARIES} ${LINK_DIR1}/libParticleSimulator.so ${LINK_DIR2}/libBOSClass.so)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
if(APPLE)
	TARGET_LINK_LIBRARIES(test.exe ${ROOT_LIBRARIES} ${LINK_DIR1}/libParticleSimulator.dylib ${LINK_DIR2}/libBOSClass.dylib)
endif(APPLE)


ADD_EXECUTABLE(read.exe "read.cxx")
IF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	TARGET_LINK_LIBRARIES(read.exe ${ROOT_LIBRARIES} ${LINK_DIR2}/libBOSClass.so)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
if(APPLE)
	TARGET_LINK_LIBRARIES(test.exe ${ROOT_LIBRARIES} ${LINK_DIR1}/libParticleSimulator.dylib ${LINK_DIR2}/libBOSClass.dylib)
endif(APPLE)
