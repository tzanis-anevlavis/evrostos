set(SRCS
  MasterPrinter.c
  PrinterBase.c
  PrinterIWffCore.c
  PrinterPsl.c
  PrinterSexpCore.c
  PrinterWffCore.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  MasterPrinter.h
  PrinterBase.h
  PrinterWffCore.h
  PrinterPsl.h
  PrinterIWffCore.h
  PrinterSexpCore.h
  PrinterBase_private.h
  )
