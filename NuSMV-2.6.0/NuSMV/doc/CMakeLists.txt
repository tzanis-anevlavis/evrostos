cmake_minimum_required(VERSION 2.8.8)
project(NuSMV-doc)

find_package(LATEX)
find_package(Doxygen)

set(run_with_latex_env "${PROJECT_SOURCE_DIR}/../cmake/run_with_latex_env.py")

if(NOT DOXYGEN_FOUND)
   
   add_subdirectory(user-man)
   add_subdirectory(tutorial)
   add_subdirectory(html)
   add_subdirectory(prog-man)

   
   add_custom_target(${DOCS_TARGET_PREFIX}docs
     COMMAND ${CMAKE_COMMAND} -E echo "Warning: Doxygen not found, prog-man documentation cannot be built."
     DEPENDS ${DOCS_TARGET_PREFIX}user-man
             ${DOCS_TARGET_PREFIX}tutorial
             ${DOCS_TARGET_PREFIX}html
             )
     
else()
   add_subdirectory(user-man)
   add_subdirectory(tutorial)
   add_subdirectory(html)
   add_subdirectory(prog-man)
   
   add_custom_target(${DOCS_TARGET_PREFIX}docs
      DEPENDS ${DOCS_TARGET_PREFIX}user-man
              ${DOCS_TARGET_PREFIX}tutorial
              ${DOCS_TARGET_PREFIX}html
              ${DOCS_TARGET_PREFIX}prog-man)
                 
endif()
