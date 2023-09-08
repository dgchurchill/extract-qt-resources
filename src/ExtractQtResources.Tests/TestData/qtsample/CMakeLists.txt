cmake_minimum_required(VERSION 3.16)

project(qtsample VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 REQUIRED COMPONENTS Core)
qt_standard_project_setup()

qt_add_executable(qtsample
    main.cpp
)

target_link_libraries(qtsample PRIVATE Qt6::Core)