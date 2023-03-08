# Recreated Goon
Recreated_goon is a code generator for automated C++ code generation directly invoked by CMake. A classic usecase for it is the generation of code for the use with remote computing and `aglio`.

## Usage
To use recreated_goon, you can include the library into your existing CMake project with `git submodules` by adding the following lines to your `CMakeLists.txt`:
```cmake
add_subdirectory(recreated_goon)
target_link_libraries(
    ${target_name} 
    recreated_goon::recreated_goon
    )
```
If you want to use the FetchContent feature set of CMake to include the library to your project just add the following lines instead:
```cmake
include(FetchContent)
FetchContent_Declare(
    recreated_goon
    GIT_REPOSITORY git@github.com:dominicpoeschko/recreated_goon.git
    GIT_TAG master
)
```

## Examples

To build the examples go into the [examples](examples) and run the following commands:
```bash
mkdir build
cd build
cmake ..
make
```