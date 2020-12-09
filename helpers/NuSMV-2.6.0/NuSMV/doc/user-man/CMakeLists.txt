set(user-man-target ${DOCS_TARGET_PREFIX}user-man)

foreach(prog
        PDFLATEX_COMPILER BIBTEX_COMPILER MAKEINDEX_COMPILER PS2PDF_CONVERTER)
    if(NOT ${prog})
        add_custom_target(${user-man-target}) # empty target
        message(WARNING "${prog} not found, documentation can't be built")
        return()
    endif()
endforeach()

set(IF_USE_PACKAGE_HYPERREF "\\iftrue")
string(REGEX MATCH "^[0-9]+[.][0-9]+" NUSMV_MAJ_VER "${PACKAGE_VERSION}")
configure_file(main.tex.in main.tex @ONLY)
configure_file(nusmv.sty.in nusmv.sty @ONLY)

file(GLOB cmd_files RELATIVE .
  "cmd/*.tex" "cmd/*.fig" "cmd/*.dia" "cmd/*.eps" "cmd/*.sty"
  "cmd/*.ist" "cmd.*.bib")

set(user_man_files
    app.tex
    app_grammar.tex
    batch.tex
    citeall.tex
    typerulesapp.tex
    epsf.tex
    index.sty
    inter.tex
    intro.tex
    "${CMAKE_CURRENT_BINARY_DIR}/main.tex"
    nusmv.ist
    "${CMAKE_CURRENT_BINARY_DIR}/nusmv.sty"
    syntax.tex
    title.tex
    ${cmd_files}
    "${CMAKE_CURRENT_BINARY_DIR}/cmdpo.pdf"
    )

add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/nusmv.pdf"
  COMMAND "${PYTHON_EXECUTABLE}"
          "${run_with_latex_env}"
          "${CMAKE_CURRENT_SOURCE_DIR}" "${PDFLATEX_COMPILER}" main
  COMMAND "${CMAKE_COMMAND}" -E rename main.pdf nusmv.pdf
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  VERBATIM
  DEPENDS ${user_man_files} "${CMAKE_CURRENT_BINARY_DIR}/main.bbl"
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


add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/cmdpo.pdf"
  COMMAND "${PS2PDF_CONVERTER}" "${CMAKE_CURRENT_SOURCE_DIR}/cmdpo.eps"
           "${CMAKE_CURRENT_BINARY_DIR}/cmdpo.pdf"
  VERBATIM
  DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/cmdpo.eps"
  )


add_custom_target(${user-man-target}
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/nusmv.pdf"
  COMMENT "Built user manual in ${CMAKE_CURRENT_BINARY_DIR}/nusmv.pdf"
  )
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/nusmv.pdf"
        DESTINATION share/nusmv/doc
        OPTIONAL)
