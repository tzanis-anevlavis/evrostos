# Build the official source distribution with its own Meson build. CUDD and
# upstream's CUDD patches are bundled in the checksum-verified archive.
include(FetchContent)
include(ExternalProject)
find_program(EVROSTOS_MESON_EXECUTABLE meson REQUIRED)
find_program(EVROSTOS_NINJA_EXECUTABLE ninja REQUIRED)
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(Python3 3 REQUIRED COMPONENTS Interpreter)
if(POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()
FetchContent_Declare(nusmv_source
    URL https://nusmv.fbk.eu/distrib/2.7.1/NuSMV-2.7.1.tar.xz
    URL_HASH SHA256=f1e11931f71d98aa9b84181eed67db584d7111100c2e967c904a31c15f823f60)
FetchContent_MakeAvailable(nusmv_source)

# Pass tool paths through Meson's native-file interface, including Python on
# systems where only python3 is on PATH. No changes to upstream sources or flags.
set(nusmv_native "${CMAKE_CURRENT_BINARY_DIR}/nusmv-native.ini")
foreach(tool IN ITEMS CMAKE_C_COMPILER BISON_EXECUTABLE FLEX_EXECUTABLE Python3_EXECUTABLE)
    if("${${tool}}" MATCHES "['\n\r]")
        message(FATAL_ERROR "NuSMV build tool paths cannot contain quotes or newlines")
    endif()
endforeach()
# Preserve the timestamp when tool paths are unchanged. Meson tracks this
# file as a configuration input.
file(CONFIGURE OUTPUT "${nusmv_native}" CONTENT [=[
[binaries]
c = '@CMAKE_C_COMPILER@'
bison = '@BISON_EXECUTABLE@'
flex = '@FLEX_EXECUTABLE@'
python = '@Python3_EXECUTABLE@'
]=] @ONLY)
get_filename_component(nusmv_ninja_dir "${EVROSTOS_NINJA_EXECUTABLE}" DIRECTORY)
get_filename_component(nusmv_meson_dir "${EVROSTOS_MESON_EXECUTABLE}" DIRECTORY)
set(nusmv_environment "PATH=${nusmv_meson_dir}:${nusmv_ninja_dir}:$ENV{PATH}")
set(nusmv_binary "${CMAKE_CURRENT_BINARY_DIR}/nusmv-build")
ExternalProject_Add(evrostos_nusmv_build
    SOURCE_DIR "${nusmv_source_SOURCE_DIR}"
    BINARY_DIR "${nusmv_binary}"
    DOWNLOAD_COMMAND ""
    UPDATE_COMMAND ""
    CONFIGURE_COMMAND "${CMAKE_COMMAND}" -E env "${nusmv_environment}"
        "${EVROSTOS_MESON_EXECUTABLE}" setup --reconfigure <BINARY_DIR> <SOURCE_DIR>
        --native-file "${nusmv_native}" --buildtype=release --wrap-mode=nodownload
        --force-fallback-for=cudd-2.4.1
        -Dbuild-universal-binary=disabled -Dbuild-doc=disabled
        -Dwith-shell=enabled -Dwith-bignumbers=enabled -Dwith-libxml2=enabled
        -Dwith-minisat=disabled -Dwith-compass=enabled -Dwith-ltl2smv=disabled
        -Dwith-gtest=disabled -Dwith-profiler=disabled -Dwith-watchdog=disabled
    # The environment wrapper closes Make's jobserver descriptors. Let the
    # nested Ninja schedule its own jobs when building through this wrapper.
    BUILD_COMMAND "${CMAKE_COMMAND}" -E env --unset=MAKEFLAGS "${nusmv_environment}"
        "${EVROSTOS_NINJA_EXECUTABLE}" -C <BINARY_DIR> NuSMV
    INSTALL_COMMAND ""
    BUILD_BYPRODUCTS "${nusmv_binary}/NuSMV")
add_executable(evrostos_nusmv_tool IMPORTED GLOBAL)
set_target_properties(evrostos_nusmv_tool PROPERTIES IMPORTED_LOCATION "${nusmv_binary}/NuSMV")
add_dependencies(evrostos_nusmv_tool evrostos_nusmv_build)
