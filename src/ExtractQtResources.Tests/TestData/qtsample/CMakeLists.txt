cmake_minimum_required(VERSION 3.16.0)

project(qtsample VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTORCC ON)

find_package(Qt6 COMPONENTS Core REQUIRED)

add_executable(qtsample
    main.cpp
    resources.qrc
)

target_link_libraries(qtsample PRIVATE Qt6::Core)