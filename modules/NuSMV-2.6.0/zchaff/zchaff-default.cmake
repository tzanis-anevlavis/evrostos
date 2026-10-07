# ----------------------------------------------------------------------
# ZChaff settings for NuSMV
#
# ** WARNING ** DO NOT CHANGE THIS FILE!
# Instead copy it to 'zchaff-default.cmake' in your build directory and modify
# your local version if you need to customize it.
# ----------------------------------------------------------------------

# this is for the older version
#set(zchaff_ver 2004.11.15)
#set(zchaff_zip zchaff.${zchaff_ver}.zip)
#set(zchaff_dir zchaff)

set(zchaff_ver 2007.3.12)
set(zchaff_zip zchaff.64bit.${zchaff_ver}.zip)
set(zchaff_url "http://www.princeton.edu/~chaff/zchaff/${zchaff_zip}")
set(zchaff_dir zchaff64)

# ----------------------------------------------------------------------
set(zchaff_patch zchaff.64bit.${zchaff_ver}_nusmv.patch)
set(zchaff_link zchaff_v${zchaff_ver})
set(zchaff_libname sat)
set(zchaff_ifcname SAT_C.h)

set(patched_tag patched_${zchaff_ver})
