set(SUBDIRS
    prob
    ap
    )
foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()
