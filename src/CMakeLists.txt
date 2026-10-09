add_library(evrostos_core STATIC
    evrostos.cpp
    detail/parser.cpp
    detail/translator.cpp
    backends/serialization.cpp
)
add_library(evrostos::core ALIAS evrostos_core)
target_include_directories(evrostos_core PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_features(evrostos_core PUBLIC cxx_std_20)
evrostos_compile_options(evrostos_core)
