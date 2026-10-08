cmake_minimum_required(VERSION 3.20)

project(Evrostos LANGUAGES CXX)
set(CMAKE_CXX_EXTENSIONS OFF)

option(EVROSTOS_ENABLE_SANITIZERS "Enable address and undefined-behavior sanitizers" OFF)

function(evrostos_compile_options target)
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
        target_compile_options(${target} PRIVATE -Wall -Wextra -Wpedantic -Wconversion -Wshadow)
        if(EVROSTOS_ENABLE_SANITIZERS)
            target_compile_options(${target} PRIVATE -fsanitize=address,undefined -fno-omit-frame-pointer)
            target_link_options(${target} PUBLIC -fsanitize=address,undefined)
        endif()
    elseif(MSVC)
        target_compile_options(${target} PRIVATE /W4 /permissive-)
    endif()
endfunction()

if(EVROSTOS_ENABLE_SANITIZERS AND NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    message(FATAL_ERROR "EVROSTOS_ENABLE_SANITIZERS requires Clang or GCC")
endif()

add_subdirectory(src)

include(CTest)
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()
