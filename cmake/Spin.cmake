# SPIN 6.5.2 release tag, resolved to an immutable upstream commit.
include(FetchContent)
find_package(BISON REQUIRED)
if(POLICY CMP0135)
    # Extraction-time timestamps make changed archives trigger dependent rebuilds.
    cmake_policy(SET CMP0135 NEW)
endif()
# The immutable revision selects the sources; the hash verifies the archive bytes.
# FETCHCONTENT_SOURCE_DIR_SPIN_SOURCE can supply an already-extracted offline copy.
FetchContent_Declare(spin_source
    URL https://github.com/nimble-code/Spin/archive/4883fbb1b1ef1f75fa9dda78efe1ad8910baf819.tar.gz
    URL_HASH SHA256=6f4963a4d6ca38f1af9ceaa76a815fbbd92e7ed7f2c424f1af88f67ec3f289f6)
FetchContent_MakeAvailable(spin_source)

# Generate the parser and compile unchanged sources entirely in the build tree.
set(spin_src "${spin_source_SOURCE_DIR}/Src")
set(spin_generated "${CMAKE_CURRENT_BINARY_DIR}/spin-generated")
file(MAKE_DIRECTORY "${spin_generated}")
# Upstream C sources include y.tab.h by name, so retain that generated header name.
BISON_TARGET(SpinParser "${spin_src}/spin.y" "${spin_generated}/spin.tab.c"
    DEFINES_FILE "${spin_generated}/y.tab.h")
# Compile the upstream source list directly, without running its in-source Makefile.
set(spin_sources spinlex sym vars main msc_tcl mesg flow sched run
    pangen1 pangen2 pangen3 pangen4 pangen5 guided dstep structs pangen6 pangen7 reprosrc
    tl_parse tl_lex tl_main tl_trans tl_buchi tl_mem tl_rewrt tl_cache)
list(TRANSFORM spin_sources PREPEND "${spin_src}/")
list(TRANSFORM spin_sources APPEND ".c")
add_executable(evrostos_spin_tool ${spin_sources} ${BISON_SpinParser_OUTPUTS})
target_include_directories(evrostos_spin_tool PRIVATE "${spin_src}" "${spin_generated}")
# NXT enables X in SPIN's LTL parser. The adapter disables statement merging during
# verifier generation and partial-order reduction during verifier compilation.
target_compile_definitions(evrostos_spin_tool PRIVATE NXT)
set_target_properties(evrostos_spin_tool PROPERTIES OUTPUT_NAME spin C_STANDARD 99 C_EXTENSIONS ON)
if(APPLE)
    # Select upstream's macOS-specific implementation paths.
    target_compile_definitions(evrostos_spin_tool PRIVATE MAC)
endif()
