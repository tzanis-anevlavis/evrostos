find_package(Python3 3.10 REQUIRED COMPONENTS Interpreter)
find_package(Threads REQUIRED)

include(FetchContent)
if(POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()
set(BUILD_GMOCK OFF)
set(INSTALL_GTEST OFF)
if(MSVC)
    set(gtest_force_shared_crt ON)
endif()
FetchContent_Declare(googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.18.0.tar.gz
    URL_HASH SHA256=6e3191c1455468b3fc35a417fb565c1c5071aee1b7e7f85e30cf48a98d37d8b5)
FetchContent_MakeAvailable(googletest)

add_executable(core_test core_test.cpp)
target_link_libraries(core_test PRIVATE evrostos::core Threads::Threads GTest::gtest_main)
evrostos_compile_options(core_test)
include(GoogleTest)
gtest_discover_tests(core_test
    TEST_PREFIX "core.api."
    PROPERTIES TIMEOUT 60 LABELS "unit")

add_executable(serialization_test serialization_test.cpp)
target_link_libraries(serialization_test PRIVATE evrostos::core Threads::Threads GTest::gtest_main)
evrostos_compile_options(serialization_test)
gtest_discover_tests(serialization_test
    TEST_PREFIX "backends."
    PROPERTIES TIMEOUT 60 LABELS "unit;serialization")

# Private fixture bridge, built only with BUILD_TESTING. Not an application CLI.
add_executable(translation_test_driver translation_driver.cpp)
target_link_libraries(translation_test_driver PRIVATE evrostos::core)
evrostos_compile_options(translation_test_driver)

add_test(NAME semantics.reference
    COMMAND "${Python3_EXECUTABLE}" -B -m unittest discover
            -s "${CMAKE_CURRENT_SOURCE_DIR}/semantics" -p "test_*.py" -v)
add_test(NAME core.semantic_integration
    COMMAND "${Python3_EXECUTABLE}" -B "${CMAKE_CURRENT_SOURCE_DIR}/semantics/cpp_integration.py"
            "$<TARGET_FILE:translation_test_driver>")
set_tests_properties(semantics.reference core.semantic_integration PROPERTIES TIMEOUT 60)
add_test(NAME backends.serialization_integration
    COMMAND "${Python3_EXECUTABLE}" -B "${CMAKE_CURRENT_SOURCE_DIR}/semantics/serialization_integration.py"
            "$<TARGET_FILE:translation_test_driver>")
set_tests_properties(backends.serialization_integration PROPERTIES TIMEOUT 120 LABELS "serialization")

if(EVROSTOS_ENABLE_SPIN)
    # The helper supplies controlled subprocess behavior, including nested runner
    # calls. Its absolute build path is passed to the GoogleTest executable.
    add_executable(process_test_helper process_helper.cpp)
    target_compile_features(process_test_helper PRIVATE cxx_std_20)
    target_compile_definitions(process_test_helper PRIVATE _POSIX_C_SOURCE=200809L)
    target_link_libraries(process_test_helper PRIVATE evrostos::spin Threads::Threads)
    evrostos_compile_options(process_test_helper)
    add_executable(process_test process_test.cpp)
    target_compile_definitions(process_test PRIVATE _POSIX_C_SOURCE=200809L
        "$<$<PLATFORM_ID:Darwin>:_DARWIN_C_SOURCE>"
        EVROSTOS_PROCESS_HELPER="$<TARGET_FILE:process_test_helper>")
    target_link_libraries(process_test PRIVATE evrostos::spin Threads::Threads GTest::gtest_main)
    add_dependencies(process_test process_test_helper)
    evrostos_compile_options(process_test)
    gtest_discover_tests(process_test TEST_PREFIX "backends."
        PROPERTIES TIMEOUT 30 LABELS "unit;spin")

    # Protocol fixtures exercise malformed evidence without launching a checker.
    add_executable(spin_result_test spin_result_test.cpp)
    target_link_libraries(spin_result_test PRIVATE evrostos::spin GTest::gtest_main)
    evrostos_compile_options(spin_result_test)
    gtest_discover_tests(spin_result_test TEST_PREFIX "backends."
        PROPERTIES TIMEOUT 30 LABELS "unit;spin")

    # Integration cases use the pinned checker and this build's C compiler.
    # Explicit dependencies make a test-target-only build prepare the checker too.
    add_executable(spin_test spin_test.cpp)
    target_compile_definitions(spin_test PRIVATE _POSIX_C_SOURCE=200809L
        "$<$<PLATFORM_ID:Darwin>:_DARWIN_C_SOURCE>"
        EVROSTOS_SPIN_EXECUTABLE="$<TARGET_FILE:evrostos_spin_tool>"
        EVROSTOS_C_COMPILER="${CMAKE_C_COMPILER}")
    target_link_libraries(spin_test PRIVATE evrostos::spin Threads::Threads GTest::gtest_main)
    add_dependencies(spin_test evrostos_spin_tool)
    evrostos_compile_options(spin_test)
    gtest_discover_tests(spin_test TEST_PREFIX "backends."
        PROPERTIES TIMEOUT 120 LABELS "integration;spin")
endif()

option(EVROSTOS_ENABLE_JAVA_DIFFERENTIAL_TESTS "Test against the repository's Java translator" OFF)
set(EVROSTOS_JAVA_REFERENCE_JAR "" CACHE FILEPATH "Optional additional Java translator JAR to test")

if(EVROSTOS_ENABLE_JAVA_DIFFERENTIAL_TESTS)
    find_package(Java 11 REQUIRED COMPONENTS Runtime Development)
    set(java_classes "${CMAKE_CURRENT_BINARY_DIR}/java-reference/classes")
    set(java_bridge "${CMAKE_CURRENT_BINARY_DIR}/java-reference/bridge")
    file(GLOB_RECURSE java_sources CONFIGURE_DEPENDS
        "${PROJECT_SOURCE_DIR}/modules/rltl2ltl/src/*.java")
    # Compile checked-in generated parser sources, without running JavaCC or Ant.
    add_custom_command(
        OUTPUT "${java_classes}/compiled.stamp"
        COMMAND "${CMAKE_COMMAND}" -E make_directory "${java_classes}"
        COMMAND "${Java_JAVAC_EXECUTABLE}" -encoding UTF-8 -d "${java_classes}" ${java_sources}
        COMMAND "${CMAKE_COMMAND}" -E touch "${java_classes}/compiled.stamp"
        DEPENDS ${java_sources}
        VERBATIM)
    add_custom_command(
        OUTPUT "${java_bridge}/LegacyBridge.class"
        COMMAND "${CMAKE_COMMAND}" -E make_directory "${java_bridge}"
        COMMAND "${Java_JAVAC_EXECUTABLE}" -encoding UTF-8 -classpath "${java_classes}"
            -d "${java_bridge}" "${CMAKE_CURRENT_SOURCE_DIR}/java/LegacyBridge.java"
        DEPENDS "${java_classes}/compiled.stamp" java/LegacyBridge.java
        VERBATIM)
    add_custom_target(java_differential_bridge ALL DEPENDS "${java_bridge}/LegacyBridge.class")

    function(evrostos_java_test name reference)
        add_test(NAME "${name}"
            COMMAND "${Python3_EXECUTABLE}" -B "${CMAKE_CURRENT_SOURCE_DIR}/semantics/java_differential.py"
                --driver "$<TARGET_FILE:translation_test_driver>"
                --java "${Java_JAVA_EXECUTABLE}" --bridge "${java_bridge}" --reference "${reference}")
        set_tests_properties("${name}" PROPERTIES TIMEOUT 600 LABELS "java;differential")
    endfunction()
    evrostos_java_test(core.java_differential "${java_classes}")
    if(EVROSTOS_JAVA_REFERENCE_JAR)
        if(NOT EXISTS "${EVROSTOS_JAVA_REFERENCE_JAR}")
            message(FATAL_ERROR "EVROSTOS_JAVA_REFERENCE_JAR does not exist")
        endif()
        evrostos_java_test(core.java_archive_differential "${EVROSTOS_JAVA_REFERENCE_JAR}")
    endif()
elseif(EVROSTOS_JAVA_REFERENCE_JAR)
    message(FATAL_ERROR "EVROSTOS_JAVA_REFERENCE_JAR requires EVROSTOS_ENABLE_JAVA_DIFFERENTIAL_TESTS")
endif()
