set(SRCS
  NodeAnonymizerST.c
  NodeAnonymizerAtom.c
  NodeAnonymizerBase.c
  NodeAnonymizerDot.c
  PrinterNonAmbiguousDot.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  NodeAnonymizerBase.h
  NodeAnonymizerBase_private.h
  NodeAnonymizerDot.h
  NodeAnonymizerDot_private.h
  NodeAnonymizerAtom.h
  NodeAnonymizerAtom_private.h
  NodeAnonymizerST.h
  NodeAnonymizerST_private.h
  PrinterNonAmbiguousDot.h
  PrinterNonAmbiguousDot_private.h
  )
