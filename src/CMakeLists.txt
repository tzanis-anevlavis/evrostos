add_library(evrostos_core STATIC
    evrostos.cpp
    verification.cpp
    detail/parser.cpp
    detail/translator.cpp
    backends/serialization.cpp
)
add_library(evrostos::core ALIAS evrostos_core)
target_include_directories(evrostos_core PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_features(evrostos_core PUBLIC cxx_std_20)
evrostos_compile_options(evrostos_core)

# Keep process execution in a separate library; the translation/serialization
# core remains usable without the native backend's platform dependencies.
if(EVROSTOS_ENABLE_SPIN OR EVROSTOS_ENABLE_NUSMV)
    find_package(Threads REQUIRED)
    add_library(evrostos_process STATIC backends/detail/process.cpp)
    target_link_libraries(evrostos_process PUBLIC evrostos::core PRIVATE Threads::Threads)
    target_compile_definitions(evrostos_process PRIVATE _POSIX_C_SOURCE=200809L)
    evrostos_compile_options(evrostos_process)
endif()
if(EVROSTOS_ENABLE_SPIN)
    add_library(evrostos_spin STATIC
        backends/spin.cpp
        backends/detail/spin_result.cpp)
    add_library(evrostos::spin ALIAS evrostos_spin)
    # Enable the POSIX declarations used by the runner and macOS's mkdtemp API.
    target_compile_definitions(evrostos_spin PRIVATE _POSIX_C_SOURCE=200809L
        "$<$<PLATFORM_ID:Darwin>:_DARWIN_C_SOURCE>")
    target_link_libraries(evrostos_spin PUBLIC evrostos::core PRIVATE evrostos_process)
    evrostos_compile_options(evrostos_spin)
endif()
if(EVROSTOS_ENABLE_NUSMV)
    add_library(evrostos_nusmv STATIC backends/nusmv.cpp backends/detail/nusmv_result.cpp)
    add_library(evrostos::nusmv ALIAS evrostos_nusmv)
    target_compile_definitions(evrostos_nusmv PRIVATE _POSIX_C_SOURCE=200809L
        "$<$<PLATFORM_ID:Darwin>:_DARWIN_C_SOURCE>")
    target_link_libraries(evrostos_nusmv PUBLIC evrostos::core PRIVATE evrostos_process)
    evrostos_compile_options(evrostos_nusmv)
endif()
