cmake_minimum_required(VERSION 2.8.8)

project(NuSMV-MiniSat)

set(MINISAT_CONFIG_FILE "" CACHE PATH "Custom config file for MiniSat")

if(EXISTS "${MINISAT_CONFIG_FILE}")
    include("${MINISAT_CONFIG_FILE}")
else()
    include(minisat-default.cmake)
endif()

if(NOT minisat_use_cmake)
    find_program(MAKE make)
    if(NOT MAKE)
        message(FATAL_ERROR "Make not found")
    endif()
endif()

find_program(PATCH patch)
if(NOT PATCH)
    message(FATAL_ERROR "Patch not found")
endif()


# download the archive first
if(NOT EXISTS "${PROJECT_BINARY_DIR}/${minisat_zip}")
    if(EXISTS "${PROJECT_SOURCE_DIR}/${minisat_zip}")
        message("Using MiniSat from archive ${minisat_zip}")
        file(COPY "${PROJECT_SOURCE_DIR}/${minisat_zip}"
          DESTINATION "${PROJECT_BINARY_DIR}")
    else()
        message("Downloading MiniSat from ${minisat_url} --> ${PROJECT_BINARY_DIR}/${minisat_zip}")
        file(DOWNLOAD ${minisat_url} "${PROJECT_BINARY_DIR}/${minisat_zip}"
          STATUS dwnres
          SHOW_PROGRESS
          LOG dwnlog)

        list(GET dwnres 0 status_code)
        list(GET dwnres 1 status_string)
        if(NOT status_code EQUAL 0)
          message(FATAL_ERROR "error: downloading '${minisat_url}' failed
  status_code: ${status_code}
  status_string: ${status_string}
  log: ${dwnlog}
  You can try downloading the archive manually, and then retry.
")
        endif()
    endif()
endif()

# convert newlines to the native format, otherwise patch might fail
file(WRITE "${PROJECT_BINARY_DIR}/CMakeFiles/make_minisat_patch.cmake"
  "file(READ \"${PROJECT_SOURCE_DIR}/${minisat_patch}\" _minisat_patch)
file(WRITE \"${PROJECT_BINARY_DIR}/${minisat_patch}\" \"\${_minisat_patch}\")
if(EXISTS \"${PROJECT_BINARY_DIR}/${minisat_dir}\")
    file(REMOVE_RECURSE \"${PROJECT_BINARY_DIR}/${minisat_dir}\")
endif()
if(EXISTS \"${PROJECT_BINARY_DIR}/${patched_tag}\")
    file(REMOVE \"${PROJECT_BINARY_DIR}/${patched_tag}\")
endif()
")

add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${minisat_patch}"
  COMMAND ${CMAKE_COMMAND} -P "${PROJECT_BINARY_DIR}/CMakeFiles/make_minisat_patch.cmake"
  COMMAND ${CMAKE_COMMAND} -E tar xzf ${minisat_zip}
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
  DEPENDS "${PROJECT_SOURCE_DIR}/${minisat_patch}"
  )

add_custom_command(
  OUTPUT "${PROJECT_BINARY_DIR}/${minisat_dir}/${minisat_ifcname}"
  COMMAND ${PATCH} -p0 -N < "${PROJECT_BINARY_DIR}/${minisat_patch}"
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
  DEPENDS "${PROJECT_BINARY_DIR}/${minisat_patch}"
  )

if (MSVC)
  set(minisat_lib_name
    "${PROJECT_BINARY_DIR}/${minisat_dir}/${minisat_libname}.lib")
else()
  set(minisat_lib_name
    "${PROJECT_BINARY_DIR}/${minisat_dir}/${CMAKE_STATIC_LIBRARY_PREFIX}${minisat_libname}${CMAKE_STATIC_LIBRARY_SUFFIX}")
endif()

if(minisat_use_cmake)
    if(CMAKE_BUILD_TYPE)
        set(_cur_cmake_build "${CMAKE_BUILD_TYPE}")
    else()
        set(_cur_cmake_build "Release")
    endif()
    if(CMAKE_TOOLCHAIN_FILE)
        set(_toolchain "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}")
    else()
        set(_toolchain)
    endif()
    add_custom_command(
      OUTPUT "${minisat_lib_name}"
      COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}"
              ${_toolchain}
              "-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}"
              "-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}"
              "-DCMAKE_C_FLAGS=${CMAKE_C_FLAGS}"
              "-DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}"
              -DCMAKE_BUILD_TYPE=${_cur_cmake_build} -DUSE_ZLIB=OFF .
      COMMAND "${CMAKE_COMMAND}" --build . --target lib
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${minisat_dir}"
      DEPENDS "${PROJECT_BINARY_DIR}/${minisat_dir}/${minisat_ifcname}"
      COMMENT "Building MiniSat with cmake"
      VERBATIM
      )
else()
    add_custom_command(
      OUTPUT "${minisat_lib_name}"
      COMMAND ${MAKE} lib
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${minisat_dir}"
      DEPENDS "${PROJECT_BINARY_DIR}/${minisat_dir}/${minisat_ifcname}"
      COMMENT "Building MiniSat with GNU make"
      )
endif()

add_custom_target(
  MINISAT_BUILD ALL
  DEPENDS "${PROJECT_BINARY_DIR}/${minisat_dir}/${minisat_ifcname}"
          "${minisat_lib_name}"
  )

set(MINISAT_INCLUDE_DIR "${PROJECT_BINARY_DIR}/${minisat_dir}"
    CACHE INTERNAL "")

if(NOT minisat_have_proof)
    set(minisat_have_proof 0)
endif()
set(MINISAT_HAVE_PROOF "${minisat_have_proof}" CACHE INTERNAL "")

add_library(MINISAT_LIB STATIC IMPORTED GLOBAL)
set_target_properties(MINISAT_LIB PROPERTIES
                      IMPORTED_LOCATION "${minisat_lib_name}")
install(FILES "${minisat_lib_name}" DESTINATION lib)

# write the configuration file
message("writing MiniSat config file")
file(WRITE "${PROJECT_BINARY_DIR}/minisat-config.cmake"
  "set(MINISAT_INCLUDE_DIR \"${MINISAT_INCLUDE_DIR}\")
set(MINISAT_COMPILE_FLAGS \"\")
set(MINISAT_LINK_FLAGS \"\")
set(MINISAT_LINK_LIBRARIES \"\")
set(MINISAT_LIB \"${minisat_lib_name}\")
set(MINISAT_HAVE_PROOF ${minisat_have_proof})
set(MINISAT_HAS_PROOF ${minisat_have_proof})
set(MINISAT_HAS_DEC_BUDGET 0)
set(MINISAT_HAS_PREFERRED 1)
")
