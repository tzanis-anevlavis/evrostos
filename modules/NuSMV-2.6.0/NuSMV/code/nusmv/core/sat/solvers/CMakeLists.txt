set(SRCS)

if(ENABLE_MINISAT)
    set(SRCS
        ${SRCS}
        SatMinisat.c
        )

    add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/satMiniSatIfc.h"
      COMMAND "${CMAKE_COMMAND}"
      ARGS -E copy_if_different "${MINISAT_INCLUDE_DIR}/Solver_C.h"
      "${CMAKE_CURRENT_BINARY_DIR}/satMiniSatIfc.h"
      COMMENT "generating MiniSat interface header"
      DEPENDS "${MINISAT_INCLUDE_DIR}/Solver_C.h"
      )
    
    set_source_files_properties(
      "${CMAKE_CURRENT_BINARY_DIR}/satMiniSatIfc.h"
      PROPERTIES GENERATED 1 HEADER_FILE_ONLY 1)

    include_directories("${CMAKE_CURRENT_BINARY_DIR}")
endif()

if(ENABLE_ZCHAFF)
    set(SRCS
        ${SRCS}
        SatZchaff.c
        )

    add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/satZChaffIfc.h"
      COMMAND "${CMAKE_COMMAND}"
      ARGS -E copy_if_different "${ZCHAFF_INCLUDE_DIR}/SAT_C.h"
      "${CMAKE_CURRENT_BINARY_DIR}/satZChaffIfc.h"
      COMMENT "generating ZChaff interface header"
      DEPENDS "${ZCHAFF_INCLUDE_DIR}/SAT_C.h")
      
    set_source_files_properties(
      "${CMAKE_CURRENT_BINARY_DIR}/satZChaffIfc.h"
      PROPERTIES GENERATED 1 HEADER_FILE_ONLY 1)

    include_directories("${CMAKE_CURRENT_BINARY_DIR}")
endif()

nusmv_add_pkg_lib(${SRCS})

nusmv_get_pkg_name(_curpkg)

if(ENABLE_ZCHAFF)
    add_custom_target(zchaff_interface_header
      SOURCES "${CMAKE_CURRENT_BINARY_DIR}/satZChaffIfc.h"
      DEPENDS ZCHAFF_BUILD)
    add_dependencies(${_curpkg} zchaff_interface_header)
endif()

if(ENABLE_MINISAT)
    add_custom_target(minisat_interface_header
      SOURCES "${CMAKE_CURRENT_BINARY_DIR}/satMiniSatIfc.h"
      DEPENDS MINISAT_BUILD)
    add_dependencies(${_curpkg} minisat_interface_header)
endif()

nusmv_add_pkg_headers(
  SatZchaff.h
  SatMinisat.h
  )
