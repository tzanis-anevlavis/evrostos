cmake_minimum_required(VERSION 2.8.8)
project(cudd)

include(CheckTypeSize)
check_type_size("int" _int_size)
check_type_size("long int" _long_size)
check_type_size("void *" _voidp_size)

add_definitions(-DNUSMV_SIZEOF_INT=${_int_size})
add_definitions(-DNUSMV_SIZEOF_LONG=${_long_size})
add_definitions(-DNUSMV_SIZEOF_VOID_P=${_voidp_size})

add_definitions(-DHAVE_IEEE_754 -DBSD)
if(APPLE)
    add_definitions(-D__MAC_OS_X__)
elseif(UNIX)
    add_definitions(-DUNIX)
endif()

include_directories("${PROJECT_BINARY_DIR}/include/cudd")

set(dirs
  st
  cudd
  epd
  mtr
  util)

macro(cudd_add_library name)
    string(TOUPPER ${name} _n)
    add_library(CUDD_${_n}_LIB STATIC ${ARGN})
    set_target_properties(CUDD_${_n}_LIB
      PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib"
                 ARCHIVE_OUTPUT_NAME ${name})
    install(TARGETS CUDD_${_n}_LIB DESTINATION lib)
endmacro()

macro(cudd_add_headers)
    get_property(_h DIRECTORY "${PROJECT_SOURCE_DIR}" PROPERTY headers)
    foreach(_a ${ARGV})
        set(_h ${_h} "${CMAKE_CURRENT_SOURCE_DIR}/${_a}")
    endforeach()
    set_property(DIRECTORY "${PROJECT_SOURCE_DIR}"
                 PROPERTY headers ${_h})
    unset(_h)
endmacro()

foreach(_d ${dirs})
    add_subdirectory(${_d})
endforeach()

get_directory_property(_headers headers)
set(outheaders)
foreach(_h ${_headers})
    get_filename_component(_n ${_h} NAME)
    set(_o "${PROJECT_BINARY_DIR}/include/cudd/${_n}")
    configure_file("${_h}" "${_o}" COPY_ONLY)
    install(FILES "${_h}" DESTINATION include/cudd)
endforeach()

set(CUDD_INCLUDE_DIR "${PROJECT_BINARY_DIR}/include" CACHE INTERNAL "")

set(cuddlibs
  CUDD_CUDD_LIB
  CUDD_EPD_LIB
  CUDD_MTR_LIB
  CUDD_ST_LIB
  CUDD_UTIL_LIB
  )

# write the configuration file for cudd
message("writing CuDD config file")
file(WRITE "${PROJECT_BINARY_DIR}/cudd-config.cmake"
  "set(CUDD_INCLUDE_DIR \"${CUDD_INCLUDE_DIR}\")
set(CUDD_COMPILE_FLAGS \"\")
set(CUDD_LINK_FLAGS \"\")
set(CUDD_LINK_LIBRARIES \"\")
"
  )
foreach(_l ${cuddlibs})
    get_target_property(_loc ${_l} LOCATION)
    file(APPEND "${PROJECT_BINARY_DIR}/cudd-config.cmake"
      "set(${_l} \"${_loc}\")
")
endforeach()
