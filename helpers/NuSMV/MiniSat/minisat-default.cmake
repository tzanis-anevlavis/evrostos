# ----------------------------------------------------------------------
# Minisat settings for NuSMV
#
# ** WARNING ** DO NOT CHANGE THIS FILE!
# Instead copy it to 'minisat-default.cmake' in your build directory
# and modify your local version if you need to customize it.
# ----------------------------------------------------------------------

set(minisat_use_cmake 0)

# This was for older version 1.14
#set(minisat_ver 1.14)
#set(minisat_zip MiniSat_v${minisat_ver}_src.zip)
#set(minisat_dir MiniSat_v${minisat_ver})

# This was for version 070721
# set(minisat_ver 070721)
# set(minisat_zip minisat2-${minisat_ver}.zip)
# set(minisat_url http://minisat.se/downloads/${minisat_zip})
# set(minisat_dir minisat)

# This was for version 2.2.0 
# set(minisat_ver 2.2.0)
# set(minisat_zip minisat-${minisat_ver}.tar.gz)
# set(minisat_url http://minisat.se/downloads/${minisat_zip})
# set(minisat_dir minisat-2.2.0)

# This is for version 37dc6c67e2af26379d88ce349eb9c4c6160e8543 on GitHub
set(minisat_ver 37dc6c6)
set(minisat_zip 37dc6c67e2af26379d88ce349eb9c4c6160e8543.zip)
set(minisat_url https://github.com/niklasso/minisat/archive/${minisat_zip})
set(minisat_dir minisat-37dc6c67e2af26379d88ce349eb9c4c6160e8543)
set(minisat_use_cmake 1)

# This is for our extension of version
# 37dc6c67e2af26379d88ce349eb9c4c6160e8543 to add proof-logging support
# set(minisat_ver 37dc6c6-proof)
# set(minisat_zip 37dc6c67e2af26379d88ce349eb9c4c6160e8543.zip)
# set(minisat_url https://github.com/niklasso/minisat/archive/${minisat_zip})
# set(minisat_dir minisat-37dc6c67e2af26379d88ce349eb9c4c6160e8543)
# set(minisat_use_cmake 1)
# set(minisat_have_proof 1)

# ----------------------------------------------------------------------
set(minisat_patch MiniSat_v${minisat_ver}_nusmv.patch)
set(minisat_link MiniSat_v${minisat_ver})
set(minisat_libname MiniSat)
set(minisat_ifcname Solver_C.h)

set(patched_tag patched_${minisat_ver})
