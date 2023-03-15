cmake_minimum_required(VERSION 3.18)

if(TARGET recreated_goon_generator)
  return()
endif()

if(NOT CMAKE_CROSSCOMPILING)

  project(recreated_goon VERSION 0.2.0)

  find_package(LLVM REQUIRED)
  find_package(Clang REQUIRED)
  mark_as_advanced(LLVM_DIR Clang_DIR FORCE)

  include(cmake_helpers/BuildOptions.cmake)

  add_executable(
    recreated_goon_generator
    src/generator.cpp
  )

  target_include_directories(recreated_goon_generator SYSTEM PRIVATE ${LLVM_INCLUDE_DIR})
  target_add_default_build_options(recreated_goon_generator PRIVATE)
  target_link_directories(recreated_goon_generator PRIVATE ${LLVM_LIBRARY_DIR})
  target_link_libraries(recreated_goon_generator PRIVATE clang-cpp LLVM)

else()
  include(cmake_helpers/HostBuild.cmake)
  configure_host_build(recreated_goon_generator)
endif()

find_program(clang-format clang-format)
mark_as_advanced(clang-format FORCE)

function(recreated_goon_generate targetname)

  cmake_parse_arguments(
    PARSE_ARGV
    1
    PARSED_ARGS
    "IGNORE_ERRORS;NO_ADD_CLANG_INCLUDES"
    "OUTPUT_DIRECTORY"
    "FILES"
  )

  if(PARSED_ARGS_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "unknown argument ${PARSED_ARGS_UNPARSED_ARGUMENTS}")
  endif()

  if(NOT PARSED_ARGS_OUTPUT_DIRECTORY)
    message(FATAL_ERROR "recreated_goon needs OUTPUT_DIRECTORY")
  endif()

  if(NOT PARSED_ARGS_FILES)
    message(FATAL_ERROR "recreated_goon needs FILES")
  endif()

  if(NOT PARSED_ARGS_IGNORE_ERRORS)
    set(PARSED_ARGS_IGNORE_ERRORS False)
  endif()

  if(NOT PARSED_ARGS_NO_ADD_CLANG_INCLUDES)
    if(NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
      set(PARSED_ARGS_NO_ADD_CLANG_INCLUDES False)
    else()
      set(PARSED_ARGS_NO_ADD_CLANG_INCLUDES True)
    endif()
  endif()

  set(output_directory_name ${CMAKE_CURRENT_BINARY_DIR}/recreated_goon_generated/${PARSED_ARGS_OUTPUT_DIRECTORY})

  set(input_filenames ${PARSED_ARGS_FILES})
  list(TRANSFORM input_filenames PREPEND ${CMAKE_CURRENT_SOURCE_DIR}/ OUTPUT_VARIABLE input_filenames)

  set(output_filenames)
  foreach(filename ${input_filenames})
    get_filename_component(filename_no_dir ${filename} NAME)
    set(output_filename ${output_directory_name}/TypeDescriptor_${filename_no_dir})
    list(APPEND output_filenames ${output_filename})
  endforeach()

  get_target_property(build_dir recreated_goon_generator BINARY_DIR)

  if(NOT CMAKE_CROSSCOMPILING)
    set(recreated_goon_generator_bin ${build_dir}/recreated_goon_generator)
  else()
    set(recreated_goon_generator_bin ${build_dir}/host_build/recreated_goon_generator)
  endif()

  set(optional_args)
  if(NOT PARSED_ARGS_NO_ADD_CLANG_INCLUDES)
    execute_process(
      COMMAND clang++ --print-resource-dir
      OUTPUT_VARIABLE clang_builtin_include_path
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    list(APPEND optional_args --extra-arg-before=-I${clang_builtin_include_path}/include)
  endif()
  if(PARSED_ARGS_IGNORE_ERRORS)
    list(APPEND optional_args --ignore-errors)
  endif()

  if(NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    list(APPEND optional_args --extra-arg-before=-Qunused-arguments)
  endif()

  set(
    command ${recreated_goon_generator_bin}
    -p=${CMAKE_CURRENT_BINARY_DIR}
    --output-directory=${output_directory_name}
    --extra-arg-before=-Wno-unknown-warning-option
    --extra-arg-before=-ferror-limit=0
    --extra-arg-before=-x
    --extra-arg-before=c++-header
    --extra-arg-before=-std=c++2b ##//TODO clang does not want 23 when driver is gcc
    ${optional_args}
    ${input_filenames}
  )

  if(NOT EXISTS ${clang-format})
    add_custom_command(
      COMMAND ${command}
      DEPENDS recreated_goon_generator ${input_filenames} ${recreated_goon_generator_bin}
      OUTPUT ${output_filenames}
    )
  else()
    add_custom_command(
      COMMAND ${command}
      COMMAND ${clang-format} --Wno-error=unknown  -i ${output_filenames}
      DEPENDS recreated_goon_generator ${input_filenames} ${recreated_goon_generator_bin}
      OUTPUT ${output_filenames}
    )
  endif()

  if(${CMAKE_VERSION} VERSION_LESS "3.19.0")
    set_source_files_properties(
      ${output_filenames}
      PROPERTIES GENERATED TRUE
    )
    add_custom_target(
      ${targetname}_helper
      DEPENDS ${output_filenames}
    )
    add_library(${targetname} INTERFACE)
    add_dependencies(${targetname} ${targetname}_helper)
  else()
    add_library(${targetname} INTERFACE ${output_filenames})
  endif()

  target_include_directories(${targetname} INTERFACE ${output_directory_name})

endfunction()
