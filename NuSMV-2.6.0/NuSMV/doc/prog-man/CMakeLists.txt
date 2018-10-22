set(prog-man-target ${DOCS_TARGET_PREFIX}prog-man)
cmake_minimum_required(VERSION 2.8.8)

 
find_package(Doxygen)
if(NOT DOXYGEN_FOUND)
    add_custom_target(${prog-man-target}
                           ${CMAKE_COMMAND} -E echo "Error: Doxygen not found, prog-man documentation cannot be built."
                           COMMAND exit 1
                     )
else()
    set( doxyfile_in          ${CMAKE_CURRENT_SOURCE_DIR}/doxygen_nusmv.conf.in      )
    set( doxyfile_html        ${PROJECT_BINARY_DIR}/prog-man/doxygen_nusmv_html.conf )
    set( doxyfile_latex       ${PROJECT_BINARY_DIR}/prog-man/doxygen_nusmv_latex.conf)
    set( doxy_html_index_file ${CMAKE_CURRENT_BINARY_DIR}/html/index.html            )
    set( doxy_latex_dir       ${CMAKE_CURRENT_BINARY_DIR}/latex/                     )
    set( doxy_output_root     ${CMAKE_CURRENT_BINARY_DIR}                            ) # Pasted into Doxyfile.in
    set( doxy_input           ${PROJECT_SOURCE_DIR}                                  ) # Pasted into Doxyfile.in

    configure_file( ${doxyfile_in} ${doxyfile_html} @ONLY )
    configure_file( ${doxyfile_in} ${doxyfile_latex} @ONLY )
    
    #now both doxyfile_html and doxyfile_latex have GENERATE_LATEX=NO and GENERATE_HTML=YES  
    #I need to set doxyfile_latex with GENERATE_LATEX=YES GENERATE_HTML=NO
    #To do this is enough to add GENERATE_LATEX=YES\nGENERATE_HTML=NO at the end of the file, it overrides the previous setting
    file(APPEND ${CMAKE_CURRENT_BINARY_DIR}/doxygen_nusmv_latex.conf "GENERATE_LATEX=YES\nGENERATE_HTML=NO")

    
   
   
    #for make prog-man-latex
    add_custom_command(OUTPUT ${doxy_latex_dir}
                         COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile_latex}
                         MAIN_DEPENDENCY ${doxyfile_latex} ${doxyfile_in}
                         COMMENT "Generating Latex documentation"
                      )

    add_custom_target(prog-man-latex
                        DEPENDS "${doxy_latex_dir}"
                        COMMENT "Built tutorial in ${doxy_latex_dir}"
                     )



    #for make prog-man-html or make prog-man 
    add_custom_command(OUTPUT ${doxy_html_index_file}
                         COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile_html}
                         MAIN_DEPENDENCY ${doxyfile_html} ${doxyfile_in}
                         COMMENT "Generating HTML documentation"
                      )
    
    add_custom_target(${prog-man-target}
                        DEPENDS "${doxy_html_index_file}"
                        COMMENT "Built tutorial in ${doxy_html_index_file}"
                     )
    add_custom_target(prog-man-html 
                        DEPENDS ${prog-man-target})
    
    
    
    install( DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html DESTINATION ${PROJECT_BINARY_DIR} )
endif()
