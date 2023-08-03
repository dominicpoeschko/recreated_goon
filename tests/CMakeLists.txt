find_package(Python3 COMPONENTS Interpreter REQUIRED)

get_target_property(build_dir recreated_goon_generator BINARY_DIR)

if(NOT CMAKE_CROSSCOMPILING)
  set(recreated_goon_generator_bin ${build_dir}/recreated_goon_generator)
else()
  set(recreated_goon_generator_bin
      ${build_dir}/host_build/recreated_goon_generator)
endif()

add_test(
  NAME simple_file_test
  COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/simple_file_test.py
          ${recreated_goon_generator_bin})
