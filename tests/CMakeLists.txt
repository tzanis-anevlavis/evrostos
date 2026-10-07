find_package(Python3 3.10 REQUIRED COMPONENTS Interpreter)
find_package(Threads REQUIRED)

add_executable(core_test core_test.cpp)
target_link_libraries(core_test PRIVATE evrostos::core Threads::Threads)
evrostos_compile_options(core_test)
add_test(NAME core.api COMMAND core_test)

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
set_tests_properties(core.api semantics.reference core.semantic_integration PROPERTIES TIMEOUT 60)

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
