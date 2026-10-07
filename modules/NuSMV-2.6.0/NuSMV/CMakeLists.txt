cmake_minimum_required(VERSION 2.8.8)
cmake_policy(VERSION 2.8)

project(NuSMV)
set(PROJECT_VERSION "2.6.0")

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
include(NuSMVUtils)

#-----------------------------------------------------------------------------
# User options
#-----------------------------------------------------------------------------
option(ENABLE_SHELL "Builds the interactive mode of nusmv" ON)
option(ENABLE_LTL2SMV "Builds the executable ltl2smv" ON)
option(ENABLE_PSLPARSER "Builds the executable pslparser to parse PSL properties" OFF)
option(ENABLE_LIBXML2 "Links NuSMV with libxml2 library" ON)
option(ENABLE_BIGNUMBERS "Use unlimited words width" OFF)
option(ENABLE_READLINE "Links NuSMV with readline library" ON)
option(ENABLE_BMCPROFILER "Links NuSMV with the bmc profiler library" OFF)
option(ENABLE_WATCHDOG "Links NuSMV with the watchdog library" OFF)
option(ENABLE_ZCHAFF "Makes the ZChaff SAT solver usable from within NuSMV" OFF)
option(ENABLE_MINISAT "Makes the MiniSat SAT solver usable from within NuSMV" ON)
option(ENABLE_COMPASS "Enable the Compass addons" ON)
option(ENABLE_STATIC_LINK "Link all executables statically" OFF)
option(PREFER_STATIC_LIBRARIES  "If true, prefer static version of external libraries for linking" OFF)
option(BUILD_CMD_HELP  "If true, array of command help is added in cmdHelp.c, else it remain empty" ON)
option(NO_POINTER_ORDERING "Avoid pointer comparisons for ordering (a huge source of nondeterminism on modern machines)" ON)
option(PACKAGE_MODE "Enable when creating packages, e.g. to avoid hardcoding of paths" OFF)

set(LIBXML2_LIB_DIR "" CACHE PATH "The directory which the libxml2 library is contained in")
set(GMP_INCLUDE_DIR "" CACHE PATH "The directory which the libgmp header is contained in")
set(GMP_LIB_DIR "" CACHE PATH "The directory which the libgmp library is contained in")
set(BMCPROFILER_INCLUDE_DIR "" CACHE PATH "The directory which the libprofiling header is contained in")
set(BMCPROFILER_LIB_DIR "" CACHE PATH "The directory which the libprofiling.a library is contained in")
set(WATCHDOG_INCLUDE_DIR "" CACHE PATH "The directory which the libwatchdog header is contained in")
set(WATCHDOG_LIB_DIR "" CACHE PATH "The directory which the libwatchdog library is contained in")

set(CUDD_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../cudd-2.4.1.1"
    CACHE PATH "Source directory of CuDD")
set(CUDD_BUILD_DIR "${PROJECT_BINARY_DIR}/build-cudd"
    CACHE PATH "Build directory of CuDD")

set(ZCHAFF_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../zchaff"
    CACHE PATH "Source directory of ZChaff")
set(ZCHAFF_BUILD_DIR "${PROJECT_BINARY_DIR}/build-zchaff"
    CACHE PATH "The directory where ZChaff has been built")

set(MINISAT_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../MiniSat"
    CACHE PATH "Source directory of MiniSat")
set(MINISAT_BUILD_DIR "${PROJECT_BINARY_DIR}/build-MiniSat"
    CACHE PATH "The directory where MiniSat has been built")

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# configure and build the dependencies first
#-----------------------------------------------------------------------------

if(ENABLE_STATIC_LINK)
    set(PREFER_STATIC_LIBRARIES 1 CACHE INTERNAL "")
endif()

if(PREFER_STATIC_LIBRARIES)
    list(INSERT CMAKE_FIND_LIBRARY_SUFFIXES 0 ${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

add_subdirectory(${CUDD_SOURCE_DIR} ${CUDD_BUILD_DIR})

if(ENABLE_MINISAT)
    add_subdirectory(${MINISAT_SOURCE_DIR} ${MINISAT_BUILD_DIR})
endif()

if(ENABLE_ZCHAFF)
    add_subdirectory(${ZCHAFF_SOURCE_DIR} ${ZCHAFF_BUILD_DIR})
endif()


#-----------------------------------------------------------------------------
# Check for libraries
#-----------------------------------------------------------------------------

nusmv_set_config_h_prefix("NUSMV_")
nusmv_set_config_h_str(EXEEXT "${CMAKE_EXECUTABLE_SUFFIX}")

set(nusmv_deplibs)

find_library(HAVE_LIBM m)
if(NOT HAVE_LIBM)
  if (NOT MSVC AND NOT MINGW)
    message(FATAL_ERROR "Required library m not found")
  endif()
else()
  set(nusmv_deplibs ${nusmv_deplibs} ${HAVE_LIBM})
endif()

nusmv_add_config_h_01(HAVE_LIBM)

# cudd
set(CUDD_LIBS CUDD_CUDD_LIB CUDD_EPD_LIB CUDD_MTR_LIB CUDD_ST_LIB CUDD_UTIL_LIB)
set(nusmv_deplibs ${nusmv_deplibs} ${CUDD_LIBS})

set(solver_libs)
# zchaff
if(ENABLE_ZCHAFF)
    set(solver_libs ${solver_libs} ZCHAFF_LIB)
endif()

# minisat
if(ENABLE_MINISAT)
    set(solver_libs ${solver_libs} MINISAT_LIB)
endif()

if(NOT (ENABLE_MINISAT OR ENABLE_ZCHAFF))
    message(FATAL_ERROR "Either MiniSat or Zchaff must be enabled")
endif()

set(nusmv_deplibs ${nusmv_deplibs} ${solver_libs})


# readline
if(ENABLE_READLINE AND ENABLE_SHELL)
    find_library(_readline_lib readline)
    find_library(_termcap_lib ncurses)
    if(NOT _termcap_lib)
        find_library(_termcap_lib curses)
        if(NOT _termcap_lib)
            find_library(_termcap_lib termcap)
        endif()
    endif()
    if(NOT _termcap_lib)
        message(FATAL_ERROR "Readline enabled, but no suitable library found")
    else()
        set(_readline_libs ${_readline_lib} ${_termcap_lib})
        set(HAVE_LIBREADLINE 1)

        if(PREFER_STATIC_LIBRARIES AND NOT (WIN32 OR MINGW OR ${CMAKE_SYSTEM_NAME} MATCHES "Darwin"))
            find_library(_tinfo_lib tinfo)
            if(NOT _tinfo_lib)
                message(FATAL_ERROR "tinfo lib not found, readline can't be linked statically")
            endif()
            set(_readline_libs ${_readline_libs} ${_tinfo_lib})
        endif()
    endif()

    message("linking with realine libraries: ${_readline_libs}")
    set(nusmv_deplibs ${nusmv_deplibs} ${_readline_libs})
endif()
nusmv_add_config_h_01(HAVE_LIBREADLINE)

# libxml2
if(ENABLE_LIBXML2)
    nusmv_find_libxml2(HAVE_LIBXML2 libxml2_includes libxml2_libs libxml2_defs)
    if(NOT HAVE_LIBXML2)
        set(ENABLE_LIBXML2 0 CACHE INTERNAL "")
    else()
        set(nusmv_deplibs ${nusmv_deplibs} ${libxml2_libs})
    endif()
endif()
nusmv_add_config_h_01(HAVE_LIBXML2)

# gmp
if(ENABLE_BIGNUMBERS)
    nusmv_find_path(_gmp_h gmp.h "${GMP_INCLUDE_DIR}")
    if(NOT _gmp_h)
        message(FATAL_ERROR "Bignumbers enabled, but gmp.h not found")
    endif()

    nusmv_find_library(_gmp_lib gmp "${GMP_LIB_DIR}")
    if(NOT _gmp_lib)
        message(FATAL_ERROR "Bignumbers enabled, but libgmp not found")
    endif()

    set(HAVE_LIBGMP 1)
    set(HAVE_BIGNUMBERS 1)
    set(nusmv_deplibs ${nusmv_deplibs} ${_gmp_lib})
endif()
nusmv_add_config_h_01(HAVE_LIBGMP)
nusmv_add_config_h_01(HAVE_BIGNUMBERS)

# libprofiler
if(ENABLE_BMCPROFILER)
    nusmv_find_path(_bmcprofiler_h profiling.h "${BMCPROFILER_INCLUDE_DIR}")
    if(NOT _bmcprofiler_h)
        message(FATAL_ERROR "BMCPROFILER enabled but profiling.h not found")
    endif()

    nusmv_find_library(_bmcprofiler_lib profiling "${BMCPROFILER_LIB_DIR}")
    if(NOT _bmcprofiler_lib)
        message(FATAL_ERROR "BMCPROFILER enabled but libprofiler not found")
    endif()

    set(HAVE_BMC_PROFILER_LIBRARY 1)
    set(nusmv_deplibs ${nusmv_deplibs} ${_bmcprofiler_lib})
endif()
nusmv_add_config_h_01(HAVE_BMC_PROFILER_LIBRARY)

# libwatchdog
if(ENABLE_WATCHDOG)
    nusmv_find_path(_watchdog_h watchdog.h "${WATCHDOG_INCLUDE_DIR}")
    if(NOT _watchdog_h)
        message(FATAL_ERROR "WATCHDOG enabled but watchdog.h not found")
    endif()

    nusmv_find_library(_watchdog_lib watchdog "${WATCHDOG_LIB_DIR}")
    if(NOT _watchdog_lib)
        message(FATAL_ERROR "WATCHDOG enabled but libwatchdog not found")
    endif()

    set(HAVE_WATCHDOG_LIBRARY 1)
    set(nusmv_deplibs ${nusmv_deplibs} ${_watchdog_lib})
endif()
nusmv_add_config_h_01(HAVE_WATCHDOG_LIBRARY)

#-----------------------------------------------------------------------------
# Check for features
#-----------------------------------------------------------------------------

nusmv_check_funcattr_noreturn()
nusmv_check_funcattr_warn_unused_result()
nusmv_check_malloc()
nusmv_check_common_headers()
nusmv_check_common_functions()
nusmv_check_bool()
nusmv_check_types_size()
nusmv_check_cpp()
nusmv_check_inline()


# some (for now?) hardcoded config values
nusmv_set_config_h_01(STDC_HEADERS 1)
nusmv_set_config_h_01(HAVE_CUDD_24 1)
nusmv_set_config_h_01(HAVE_SAT_SOLVER 1)
nusmv_set_config_h_01(HAVE_MINISAT_INTERPOLATION "${MINISAT_HAVE_PROOF}")
nusmv_add_config_h_01(NO_POINTER_ORDERING)

# shell
if (ENABLE_SHELL)
  set(HAVE_INTERACTIVE_SHELL 1)
endif()
nusmv_add_config_h_01(HAVE_INTERACTIVE_SHELL)


#-----------------------------------------------------------------------------
# Set info
#-----------------------------------------------------------------------------

nusmv_set_config_h_str(LIBRARY_NAME ${CMAKE_PROJECT_NAME})
nusmv_set_config_h_str(LIBRARY_VERSION ${PROJECT_VERSION})
nusmv_get_current_time(_now)
nusmv_set_config_h_str(LIBRARY_BUILD_DATE "${_now}")
nusmv_set_config_h_str(LIBRARY_WEBSITE "http://nusmv.fbk.eu")
nusmv_set_config_h_str(LIBRARY_EMAIL "nusmv-users@list.fbk.eu")
nusmv_set_config_h_str(LIBRARY_BUGREPORT "Please report bugs to <nusmv-users@fbk.eu>")

nusmv_set_config_h_str(PACKAGE_BUILD_DATE "${_now}")
nusmv_set_config_h_str(PACKAGE_BUGREPORT ${LIBRARY_BUGREPORT})
nusmv_set_config_h_str(PACKAGE_NAME ${LIBRARY_NAME})
nusmv_set_config_h_str(PACKAGE_TARNAME "nusmv")
nusmv_set_config_h_str(PACKAGE_URL "")
nusmv_set_config_h_str(PACKAGE_VERSION ${LIBRARY_VERSION})
nusmv_set_config_h_str(PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")

nusmv_set_config_h_01(HAVE_SOLVER_MINISAT ${ENABLE_MINISAT})
nusmv_set_config_h_01(HAVE_SOLVER_ZCHAFF ${ENABLE_ZCHAFF})
nusmv_set_config_h_01(HAVE_COMPASS ${ENABLE_COMPASS})

if(ENABLE_COMPASS)
    set(LINKED_CORE_ADDONS "compass")
endif()
nusmv_add_config_h_str(LINKED_CORE_ADDONS)

# if HAVE_REGEX on MinGW, check for the proper library to link
if(HAVE_REGEX_H)
    nusmv_find_library(_gnurx_lib gnurx "")
    if(_gnurx_lib)
	set(nusmv_deplibs ${nusmv_deplibs} "${_gnurx_lib}")
        message("linking with regexp library ${_gnurx_lib}")
	# under mingw, librx needs libtre:
        nusmv_find_library(_tre_lib tre "")
	if(_tre_lib)
	   set(nusmv_deplibs ${nusmv_deplibs} "${_tre_lib}")
           message("linking with regexp support library ${_tre_lib}")
        endif()
    else()
        nusmv_find_library(_regex_lib regex "")
        if(_regex_lib)
            set(nusmv_deplibs ${nusmv_deplibs} "{_regex_lib}")
            message("linking with regexp library ${_regex_lib}")
        endif()
    endif()
endif()

# under mingw, libiconv and libintl are required
if (MINGW)
   nusmv_find_library(_intl_lib intl "")
   if(_intl_lib)
       set(nusmv_deplibs ${nusmv_deplibs} "${_intl_lib}")
       message("linking with intl support library ${_intl_lib}")
   endif()
   nusmv_find_library(_iconv_lib iconv "")
   if(_iconv_lib)
       set(nusmv_deplibs ${nusmv_deplibs} "${_iconv_lib}")
       message("linking with iconv support library ${_iconv_lib}")
   endif()
endif()


#-----------------------------------------------------------------------------
# The actual build targets
#-----------------------------------------------------------------------------

if (ENABLE_SHELL)
  set(nusmv_shell_libs
    shell
    )
else()
  set(nusmv_shell_libs
    )
endif()

set(nusmv_core_libs # the order is relevant here
  addonscore
  core
  rbc
  grammar
  )

nusmv_write_config_h(nusmv-config.h)
install(FILES "${PROJECT_BINARY_DIR}/nusmv-config.h" DESTINATION include)

add_definitions(-DHAVE_CONFIG_H)


# ----------------------------------------------------------------------
# source code include dirs
set(incdirs
  "${PROJECT_BINARY_DIR}"
  "${PROJECT_BINARY_DIR}/code"
  "${PROJECT_SOURCE_DIR}/code"
  "${CUDD_INCLUDE_DIR}"
  )
if(ENABLE_BIGNUMBERS AND GMP_INCLUDE_DIR)
    set(incdirs "${incdirs}" "${GMP_INCLUDE_DIR}")
endif()
if(ENABLE_LIBXML2 AND LIBXML2_INCLUDE_DIR)
    set(incdirs "${incdirs}" "${LIBXML2_INCLUDE_DIR}")
endif()
if(ENABLE_BMCPROFILER AND BMCPROFILER_INCLUDE_DIR)
    set(incdirs "${incdirs}" "${BMCPROFILER_INCLUDE_DIR}")
endif()
if(ENABLE_WATCHDOG AND WATCHDOG_INCLUDE_DIR)
    set(incdirs "${incdirs}" "${WATCHDOG_INCLUDE_DIR}")
endif()

set(NUSMV_INCLUDE_DIRS "${incdirs}" CACHE INTERNAL "")

include_directories(BEFORE
                    ${NUSMV_INCLUDE_DIRS})

if(ENABLE_ZCHAFF)
    include_directories("${ZCHAFF_INCLUDE_DIR}")
endif()
if(ENABLE_MINISAT)
    include_directories("${MINISAT_INCLUDE_DIR}")
endif()
if(ENABLE_BIGNUMBERS AND GMP_INCLUDE_DIR)
    include_directories("${GMP_INCLUDE_DIR}")
endif()
if(ENABLE_LIBXML2)
    include_directories("${libxml2_includes}")
    add_definitions("${libxml2_defs}")
endif()
if(ENABLE_BMCPROFILER)
    include_directories("${BMCPROFILER_INCLUDE_DIR}")
endif()
if(ENABLE_WATCHDOG)
    include_directories("${WATCHDOG_INCLUDE_DIR}")
endif()
# ----------------------------------------------------------------------

set(SUBDIRS
  code/nusmv/core
  code/nusmv/addons_core
  )
if (ENABLE_SHELL)
  set(SUBDIRS
    ${SUBDIRS}
    code/nusmv/shell
    )
endif()

# for gcc, enables check for declarations after statement to be errors
if(CMAKE_COMPILER_IS_GNUCC)
  set(COMPILER_WARNINGS_AND_ERRORS
    "${COMPILER_WARNINGS_AND_ERRORS} -Wdeclaration-after-statement -Werror=declaration-after-statement")
endif()

# sets additional flags for compiler
set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS} ${COMPILER_WARNINGS_AND_ERRORS}")

foreach(d ${SUBDIRS})
  add_subdirectory(${d})
endforeach()

# create the libraries
# inner_deps is a list of inner library names
# which lib depends on.
set(corelibs)
macro(nusmv_add_library lib inner_deps)
  get_property(_pl GLOBAL PROPERTY nusmv_${lib}_pkgs)
  set(_o)
  foreach(_p ${_pl})
    set(_o ${_o} $<TARGET_OBJECTS:${_p}>)
  endforeach()

  set(_inner_deps)
  foreach(_p ${inner_deps})
    string(TOUPPER ${_p} _up)
    set(_inner_deps ${_inner_deps} NUSMV_${_up}_LIB)
  endforeach()

  string(TOUPPER ${lib} _lu)
  add_library(NUSMV_${_lu}_LIB STATIC ${_o})
  target_link_libraries(NUSMV_${_lu}_LIB ${nusmv_deplibs} ${_inner_deps})
  set_target_properties(NUSMV_${_lu}_LIB
    PROPERTIES ARCHIVE_OUTPUT_NAME nusmv${lib}
    ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
  set(corelibs ${corelibs} NUSMV_${_lu}_LIB)
  install(TARGETS NUSMV_${_lu}_LIB DESTINATION lib)
endmacro()

# warning: order is relevant here
foreach(_l ${nusmv_shell_libs})
  nusmv_add_library(${_l} "")
endforeach()

# core lib depends on the shell when calling the interactive loop from
# main function
foreach(_l ${nusmv_core_libs})
  nusmv_add_library(${_l} "${nusmv_shell_libs}")
endforeach()


# create the executables

if(ENABLE_STATIC_LINK AND CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_EXE_LINKER_FLAGS "-static ${CMAKE_EXE_LINKER_FLAGS}" CACHE INTERNAL "")
endif()

nusmv_add_executable(NuSMV code/nusmv/main.c)
target_link_libraries(NuSMV ${corelibs})

if(ENABLE_LTL2SMV)
    nusmv_add_executable(ltl2smv code/nusmv/ltl2smvMain.c)
    target_link_libraries(ltl2smv ${corelibs})
endif()

if(ENABLE_PSLPARSER)
    nusmv_add_executable(pslparser code/nusmv/core/parser/psl/psl.c)
    target_link_libraries(pslparser ${corelibs})
endif()

# generate the config file
message("writing NuSMV config file")
set(deplibs_locations)
foreach(_l ${nusmv_deplibs})
    get_target_property(_loc ${_l} LOCATION)
    if(NOT _loc)
        set(_loc "${_l}")
    endif()
    set(deplibs_locations ${deplibs_locations} "${_loc}")
endforeach()


file(WRITE "${PROJECT_BINARY_DIR}/nusmv-config.cmake"
  "set(NUSMV_INCLUDE_DIRS \"${NUSMV_INCLUDE_DIRS}\")
set(NUSMV_LINK_LIBRARIES \"${deplibs_locations}\")
set(NUSMV_HAVE_BIGNUMBERS ${HAVE_BIGNUMBERS})
set(NUSMV_HAVE_LIBXML2 ${HAVE_LIBXML2})
set(NUSMV_HAVE_MINISAT_INTERPOLATION ${HAVE_MINISAT_INTERPOLATION})
set(NUSMV_HAVE_BMC_PROFILER_LIBRARY ${HAVE_BMC_PROFILER_LIBRARY})
set(NUSMV_HAVE_WATCHDOG_LIBRARY ${HAVE_WATCHDOG_LIBRARY})
set(NUSMV_DEFINITIONS \"${libxml2_defs}\")
set(NUSMV_HAVE_INTERACTIVE_SHELL ${HAVE_INTERACTIVE_SHELL})
")
foreach(_l ${corelibs})
    get_target_property(_loc ${_l} LOCATION)
    file(APPEND "${PROJECT_BINARY_DIR}/nusmv-config.cmake"
      "set(${_l} \"${_loc}\")
")
endforeach()

if(ENABLE_BIGNUMBERS AND GMP_INCLUDE_DIR)
    set(incdirs "${incdirs}" "${GMP_INCLUDE_DIR}")
endif()
if(ENABLE_LIBXML2 AND LIBXML2_INCLUDE_DIR)
    set(incdirs "${incdirs}" "${LIBXML2_INCLUDE_DIR}")
endif()

# add the docs
if(NOT "${CMAKE_HOME_DIRECTORY}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
    set(DOCS_TARGET_PREFIX ${CMAKE_PROJECT_NAME}-)
endif()
add_subdirectory(doc)

#------------------------------------------------------------------------------
# dist and install targets
#------------------------------------------------------------------------------

add_subdirectory(contrib)
add_subdirectory(examples)

install(FILES
  LGPL-2.1 NEWS README.txt README_PLATFORMS.txt README_zChaff.txt
  README_MiniSat.txt share/nusmv/master.nusmvrc
  DESTINATION share/nusmv)

nusmv_add_uninstall_target()

set(ARCHIVE_NAME ${CMAKE_PROJECT_NAME}-${PROJECT_VERSION})

# ----------------------------------------------------------------------
# for source packaging
add_custom_target(gitarchive
  COMMAND git archive --worktree-attributes --format=tar --prefix=${ARCHIVE_NAME}/
  --output=${CMAKE_BINARY_DIR}/${ARCHIVE_NAME}.tar HEAD
  NuSMV/ MiniSat/ zchaff/ cudd-2.4.1.1/
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/..)

add_custom_target(dist
  tar --update --file ${CMAKE_BINARY_DIR}/${ARCHIVE_NAME}.tar
  --transform='s,,${ARCHIVE_NAME}/,'
  ${CMAKE_SOURCE_DIR}/../scripts/create_help.py ${CMAKE_SOURCE_DIR}/../scripts/html2text.py
  & gzip --force --best ${CMAKE_BINARY_DIR}/${ARCHIVE_NAME}.tar
  DEPENDS gitarchive
  )


# ----------------------------------------------------------------------
# for binary packaging
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)

SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "NuSMV project")
SET(CPACK_PACKAGE_VENDOR "FBK")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.txt")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LGPL-2.1")
SET(CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")

SET(CPACK_PACKAGE_INSTALL_DIRECTORY "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
SET(CPACK_GENERATOR "TGZ")

IF(WIN32 AND NOT UNIX)
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  SET(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\MyExecutable.exe")
  SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} My Famous Project")
  SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.my-project-home-page.org")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.my-personal-home-page.com")
  SET(CPACK_NSIS_CONTACT "me@my-personal-home-page.com")
  SET(CPACK_NSIS_MODIFY_PATH ON)
ELSE(WIN32 AND NOT UNIX)
  SET(CPACK_STRIP_FILES "bin/NuSMV" "bin/ltl2smv")
  SET(CPACK_SOURCE_STRIP_FILES "")
ENDIF(WIN32 AND NOT UNIX)

SET(CPACK_PACKAGE_EXECUTABLES "NuSMV" "NuSMV Executable")

set(_GEN ${CPACK_GENERATOR})  # save as CPack resets it

# to determine a sensible system name to be used in archive name
if (WIN32 AND NOT UNIX)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(_system_name "win64")
  else()
    set(_system_name "win32")
  endif()
else()
  set(_system_name "${CMAKE_SYSTEM_NAME}")
endif()

set(CPACK_PACKAGE_FILE_NAME "${ARCHIVE_NAME}-${_system_name}")
INCLUDE(CPack)

file(WRITE "${PROJECT_BINARY_DIR}/dist-manifest"
  "ARCHIVE_NAME = ${ARCHIVE_NAME}
SRC_PACKAGE_NAME = ${CMAKE_BINARY_DIR}/${ARCHIVE_NAME}.tar.gz
BIN_PACKAGE_NAME = ${CMAKE_BINARY_DIR}/${ARCHIVE_NAME}-${_system_name}.tar.gz
")

add_custom_target(distbin
  # this is a trick as cmake does not recognize internal targets (like
  # 'package') as a valid dependency otherwise (!)
  COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target package
  DEPENDS docs ${PROJECT_BINARY_DIR}/dist-manifest)

# make test support
enable_testing()
get_target_property(NuSMV_PATH NuSMV LOCATION)

add_test(NAME short
  COMMAND ${NuSMV_PATH}
  "${PROJECT_SOURCE_DIR}/examples/smv-dist/short.smv")

add_test(NAME bmc
  COMMAND ${NuSMV_PATH} -bmc
  "${PROJECT_SOURCE_DIR}/examples/bmc/barrel5.smv")
