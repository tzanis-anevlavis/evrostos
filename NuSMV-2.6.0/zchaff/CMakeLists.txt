cmake_minimum_required(VERSION 2.8.8)

project(NuSMV-ZChaff)

set(ZCHAFF_CONFIG_FILE "" CACHE PATH "Custom config file for ZChaff")

if(EXISTS "${ZCHAFF_CONFIG_FILE}")
    include("${ZCHAFF_CONFIG_FILE}")
else()
    include(zchaff-default.cmake)
endif()

find_program(MAKE make)
if(NOT MAKE)
    message(FATAL_ERROR "Make not found")
endif()

find_program(PATCH patch)
if(NOT PATCH)
    message(FATAL_ERROR "Patch not found")
endif()


# download the archive first
if(NOT EXISTS "${PROJECT_BINARY_DIR}/${zchaff_zip}")
    if(EXISTS "${PROJECT_SOURCE_DIR}/${zchaff_zip}")
        message("Using ZChaff from archive ${zchaff_zip}")
        file(COPY "${PROJECT_SOURCE_DIR}/${zchaff_zip}"
          DESTINATION "${PROJECT_BINARY_DIR}")
    else()
        message("Downloading ZChaff from ${zchaff_url} --> ${PROJECT_BINARY_DIR}/${zchaff_zip}")
        file(DOWNLOAD ${zchaff_url} "${PROJECT_BINARY_DIR}/${zchaff_zip}"
          STATUS dwnres
          SHOW_PROGRESS
          LOG dwnlog)

        list(GET dwnres 0 status_code)
        list(GET dwnres 1 status_string)
        if(NOT status_code EQUAL 0)
          message(FATAL_ERROR "error: downloading '${zchaff_url}' failed
  status_code: ${status_code}
  status_string: ${status_string}
  log: ${dwnlog}
  You can try downloading the archive manually, and then retry.
")
        endif()

    endif()
endif()

# extract the file
add_custom_command(OUTPUT "${PROJECT_BINARY_DIR}/${patched_tag}"
  COMMAND ${CMAKE_COMMAND} -E tar xzf ${zchaff_zip}
  COMMAND ${CMAKE_COMMAND} -E touch "${PROJECT_BINARY_DIR}/${patched_tag}"
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
  DEPENDS "${PROJECT_BINARY_DIR}/${zchaff_zip}"
  )

add_custom_command(
  OUTPUT "${PROJECT_BINARY_DIR}/${zchaff_dir}/${zchaff_ifcname}"
  COMMAND ${PATCH} -p1 -N < "${PROJECT_SOURCE_DIR}/${zchaff_patch}"
  COMMAND ${MAKE} ${zchaff_ifcname}
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${zchaff_dir}"
  DEPENDS "${PROJECT_BINARY_DIR}/${patched_tag}"
          "${PROJECT_SOURCE_DIR}/${zchaff_patch}"
  )

if (MSVC)
  set(zchaff_lib_name
    "${PROJECT_BINARY_DIR}/${zchaff_dir}/${zchaff_libname}.lib")
else()
  set(zchaff_lib_name "${PROJECT_BINARY_DIR}/${zchaff_dir}/${CMAKE_STATIC_LIBRARY_PREFIX}${zchaff_libname}${CMAKE_STATIC_LIBRARY_SUFFIX}")
endif()


if("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64" AND BUILD32)
  set(_xcompiling True)
else()
  set(_xcompiling False)
endif()

if(CMAKE_TOOLCHAIN_FILE OR _xcompiling)
  set(_toolchain
    CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER} AR=${CMAKE_AR} RANLIB=${CMAKE_RANLIB} CFLAGS=${CMAKE_C_FLAGS} CXXFLAGS=${CMAKE_CXX_FLAGS})
else()
  set(_toolchain)
endif()


add_custom_command(
  OUTPUT "${zchaff_lib_name}"
  COMMAND ${MAKE} libsat.a ${_toolchain}
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${zchaff_dir}"
  DEPENDS "${PROJECT_BINARY_DIR}/${zchaff_dir}/${zchaff_ifcname}"
  )

add_custom_target(
  ZCHAFF_BUILD ALL
  SOURCES "${PROJECT_BINARY_DIR}/${zchaff_dir}/${zchaff_ifcname}"
          "${zchaff_lib_name}"
  )

set(ZCHAFF_INCLUDE_DIR "${PROJECT_BINARY_DIR}/${zchaff_dir}" CACHE INTERNAL "")

add_library(ZCHAFF_LIB STATIC IMPORTED GLOBAL)
set_target_properties(ZCHAFF_LIB PROPERTIES
                      IMPORTED_LOCATION "${zchaff_lib_name}")
install(FILES "${zchaff_lib_name}" DESTINATION lib)

# write the configuration file
message("writing ZChaff config file")
file(WRITE "${PROJECT_BINARY_DIR}/zchaff-config.cmake"
  "set(ZCHAFF_INCLUDE_DIR \"${ZCHAFF_INCLUDE_DIR}\")
set(ZCHAFF_COMPILE_FLAGS \"\")
set(ZCHAFF_LINK_FLAGS \"\")
set(ZCHAFF_LINK_LIBRARIES \"\")
set(ZCHAFF_LIB \"${zchaff_lib_name}\")
")
