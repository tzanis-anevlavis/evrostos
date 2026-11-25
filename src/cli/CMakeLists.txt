include(FetchContent)
if(POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()
FetchContent_Declare(nlohmann_json
    URL https://github.com/nlohmann/json/releases/download/v3.12.0/json.tar.xz
    URL_HASH SHA256=42f6e95cad6ec532fd372391373363b62a14af6d771056dbfc86160e6dfff7aa)
FetchContent_MakeAvailable(nlohmann_json)
add_library(evrostos_cli_report STATIC report.cpp)
target_link_libraries(evrostos_cli_report PUBLIC evrostos::core PRIVATE nlohmann_json::nlohmann_json)
evrostos_compile_options(evrostos_cli_report)
add_executable(evrostos_cli main.cpp formula_list.cpp output.cpp verify.cpp)
set_target_properties(evrostos_cli PROPERTIES OUTPUT_NAME evrostos
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
target_link_libraries(evrostos_cli PRIVATE evrostos_cli_report nlohmann_json::nlohmann_json)
evrostos_compile_options(evrostos_cli)

# Each enabled adapter runs the checker produced by this build.
if(EVROSTOS_ENABLE_NUSMV)
    target_link_libraries(evrostos_cli PRIVATE evrostos::nusmv)
    target_compile_definitions(evrostos_cli PRIVATE
        EVROSTOS_NUSMV_EXECUTABLE="$<TARGET_FILE:evrostos_nusmv_tool>")
    add_dependencies(evrostos_cli evrostos_nusmv_tool)
endif()
if(EVROSTOS_ENABLE_SPIN)
    target_link_libraries(evrostos_cli PRIVATE evrostos::spin)
    target_compile_definitions(evrostos_cli PRIVATE
        EVROSTOS_SPIN_EXECUTABLE="$<TARGET_FILE:evrostos_spin_tool>"
        EVROSTOS_C_COMPILER="${CMAKE_C_COMPILER}")
    add_dependencies(evrostos_cli evrostos_spin_tool)
endif()
