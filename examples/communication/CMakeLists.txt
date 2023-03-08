cmake_minimum_required(VERSION 3.19)
project("Communication Example" VERSION 0.0.1)

include(FetchContent)
set(CMAKE_CXX_FLAGS -std=c++20)

FetchContent_Declare(
    aglio
    GIT_REPOSITORY git@github.com:dominicpoeschko/aglio.git
    GIT_TAG master
)
FetchContent_Declare(
    recreated_goon
    GIT_REPOSITORY git@github.com:dominicpoeschko/recreated_goon.git
    GIT_TAG master
)
FetchContent_Declare(
    toxic_spokes
    GIT_REPOSITORY git@github.com:dominicpoeschko/toxic_spokes.git
    GIT_TAG master
)
FetchContent_Declare(
    CRCpp
    GIT_REPOSITORY git@github.com:d-bahr/CRCpp.git
    GIT_TAG master
)

FetchContent_MakeAvailable(aglio toxic_spokes recreated_goon CRCpp)

recreated_goon_generate(
    type_descriptors
    IGNORE_ERRORS
    OUTPUT_DIRECTORY type_descriptors
    FILES src/package.hpp
)

add_executable(example src/main.cpp)

target_compile_options(example PUBLIC -std=c++20)
target_link_libraries(example PUBLIC
    aglio::aglio
    toxic_spokes::toxic_spokes
    CRCpp
    type_descriptors
)