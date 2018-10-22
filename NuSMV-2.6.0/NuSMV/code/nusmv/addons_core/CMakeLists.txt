set(SUBDIRS)

if(ENABLE_COMPASS)
    set(SUBDIRS
        ${SUBDIRS}
        compass
        )
endif()

foreach(_d ${SUBDIRS})
    add_subdirectory(${_d})
endforeach()

set(SRCS
    addonsCore.c
    )

nusmv_add_pkg_addon(${SRCS})

nusmv_add_pkg_headers(addonsCore.h)
