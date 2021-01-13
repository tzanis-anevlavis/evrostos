set(tutorial-target ${DOCS_TARGET_PREFIX}tutorial)

foreach(prog
        PDFLATEX_COMPILER BIBTEX_COMPILER MAKEINDEX_COMPILER PS2PDF_CONVERTER)
    if(NOT ${prog})
        add_custom_target(${tutorial-target}) # empty target
        message(WARNING "${prog} not found, documentation can't be built")
        return()
    endif()
endforeach()

set(IF_USE_PACKAGE_HYPERREF "\\iftrue")
string(REGEX MATCH "^[0-9]+[.][0-9]+" NUSMV_MAJ_VER "${PACKAGE_VERSION}")
configure_file(main.tex.in main.tex @ONLY)
configure_file(nusmvtut.sty.in nusmvtut.sty @ONLY)

set(tutorial_files
    bmc.tex
    citeall.tex
    ctl_mc.tex
    examples.tex
    intro.tex
    ltl_mc.tex
    "${CMAKE_CURRENT_BINARY_DIR}/main.tex"
    nusmv.ist
    "${CMAKE_CURRENT_BINARY_DIR}/nusmvtut.sty"
    simulation.tex
    title.tex
    )

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/tutorial.pdf"
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${PDFLATEX_COMPILER}" main
  COMMAND "${CMAKE_COMMAND}" -E rename main.pdf tutorial.pdf
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  VERBATIM
  DEPENDS ${tutorial_files} "${CMAKE_CURRENT_BINARY_DIR}/main.bbl"
  )

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/main.bbl"
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${PDFLATEX_COMPILER}" main
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${BIBTEX_COMPILER}" main
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${PDFLATEX_COMPILER}" main
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${MAKEINDEX_COMPILER}" -l -c -s
          "${CMAKE_CURRENT_SOURCE_DIR}/nusmv.ist" main
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${MAKEINDEX_COMPILER}" -o main.and
          main.adx
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${MAKEINDEX_COMPILER}" -o main.nnd
          main.ndx
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${PDFLATEX_COMPILER}" main
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  VERBATIM
  DEPENDS ${user_man_files}
  )


add_custom_target(${tutorial-target}
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/tutorial.pdf"
  COMMENT "Built tutorial in ${CMAKE_CURRENT_BINARY_DIR}/tutorial.pdf"
  )
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/tutorial.pdf"
        DESTINATION share/nusmv/doc
        OPTIONAL)
