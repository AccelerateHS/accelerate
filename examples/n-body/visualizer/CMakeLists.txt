#The name of the project
PROJECT(N_Body_Simulation_CUDA)

#The CMake Minimum version that is required. The FindCUDA script
#is distributed since version 2.8
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

#Searching CUDA
FIND_PACKAGE(CUDA)

set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} /usr/local/cuda/samples/common/lib/darwin/)

MESSAGE( STATUS "N_body - project is being generated" )

option(CUDA_DEVICE_DEBUGGING
  "On to enable debugging device code."
  OFF
  )

if (CUDA_DEVICE_DEBUGGING)
  set(DEVICE_DEBUGGING -G0)
endif (CUDA_DEVICE_DEBUGGING)


SET( NBODY_HEADERS
  bodysystem.h
  bodysystemcpu.h
  bodysystemcpu_impl.h
  bodysystemcuda.h
  bodysystemcuda_impl.h
  render_particles.h
  tipsy.h
)

SET( NBODY_SRC
  bodysystemcuda.cu
  nbody.cpp
  render_particles.cpp
)

MESSAGE( "NBODY_SRC = " ${NBODY_SRC} )

if (WIN32)
  if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    set( LIB_PATH ${CUDA_SDK_ROOT_DIR}/common/lib/x64/ )
    set( GLEW_NAME glew64 )
  else( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    set( LIB_PATH ${CUDA_SDK_ROOT_DIR}/common/lib/win32/ )
    set( GLEW_NAME glew32 )
  endif( CMAKE_SIZEOF_VOID_P EQUAL 8 )
else (WIN32)
  if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    set( LIB_PATH ${CUDA_SDK_ROOT_DIR}/common/lib/linux/x64_64/ )
    set( GLEW_NAME GLEW_x86_64 )
  else( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    set( LIB_PATH ${CUDA_SDK_ROOT_DIR}/common/lib/linux/i686/ )
    set( GLEW_NAME GLEW )
  endif( CMAKE_SIZEOF_VOID_P EQUAL 8 )
endif (WIN32)

#freeglut
FIND_PATH(FREEGLUT_INCLUDE_DIR NAMES GL/freeglut.h PATHS ${CUDA_SDK_ROOT_DIR}/common/inc)
if (WIN32)
  FIND_LIBRARY(FREEGLUT_LIBRARY NAMES freeglut PATHS ${LIB_PATH})
else (WIN32)
  FIND_LIBRARY(FREEGLUT_LIBRARY NAMES glut PATHS ${LIB_PATH})
endif (WIN32)

#glew
FIND_PATH(GLEW_INCUDE_DIR NAMES GL/glew.h PATHS ${CUDA_SDK_ROOT_DIR}/common/include)
if (WIN32)
  FIND_LIBRARY(GLEW_LIBRARY NAMES ${GLEW_NAME} PATHS ${LIB_PATH})
else (WIN32)
  FIND_LIBRARY(GLEW_LIBRARY NAMES ${GLEW_NAME} PATHS ${LIB_PATH})
endif (WIN32)

set(ALL_LIBRARIES ${FREEGLUT_LIBRARY} ${GLEW_LIBRARY})

# Copy DLLs
if (WIN32)
  if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    file(COPY ${CUDA_SDK_ROOT_DIR}/bin/win64/Release/freeglut.dll DESTINATION ${CMAKE_BINARY_DIR})
    file(COPY ${CUDA_SDK_ROOT_DIR}/bin/win64/Release/${GLEW_NAME}.dll DESTINATION ${CMAKE_BINARY_DIR})
  else( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    file(COPY ${CUDA_SDK_ROOT_DIR}/bin/win32/Release/freeglut.dll DESTINATION ${CMAKE_BINARY_DIR})
    file(COPY ${CUDA_SDK_ROOT_DIR}/bin/win32/Release/${GLEW_NAME}.dll DESTINATION ${CMAKE_BINARY_DIR})
  endif( CMAKE_SIZEOF_VOID_P EQUAL 8 )
endif (WIN32)

include_directories(${CUDA_SDK_ROOT_DIR}/common/inc)
include_directories(${CUDA_SDK_ROOT_DIR}/../shared/inc)

set(GENCODE -gencode=arch=compute_30,code=sm_30 -gencode=arch=compute_30,code=compute_30)
set(GENCODE ${GENCODE} -gencode=arch=compute_20,code=sm_20 -gencode=arch=compute_20,code=compute_20)

CUDA_ADD_EXECUTABLE( nbody ${NBODY_SRC} ${NBODY_HEADERS}
                     OPTIONS ${GENCODE} ${DEVICE_DEBUGGING})

target_link_libraries(nbody ${ALL_LIBRARIES})