set(SRCS
  HrcDumper.c
  HrcDumperDebug.c
  HrcDumperSmv.c
  HrcDumperXml.c
  HrcDumperAnonymizer.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  HrcDumper.h
  HrcDumper_private.h
  HrcDumperDebug.h
  HrcDumperDebug_private.h
  HrcDumperSmv.h
  HrcDumperSmv_private.h
  HrcDumperXml.h
  HrcDumperXml_private.h
  )
