cmake_minimum_required(VERSION 2.8)
project( canny )
find_package( OpenCV REQUIRED )
add_executable( canny Main.cpp Timing.c )
target_link_libraries( canny ${OpenCV_LIBS} )

project( canny-gpu )
find_package( OpenCV REQUIRED )
add_executable( canny-gpu MainGPU.cpp Timing.c )
target_link_libraries( canny-gpu ${OpenCV_LIBS} )

set(CMAKE_BUILD_TYPE Release)

