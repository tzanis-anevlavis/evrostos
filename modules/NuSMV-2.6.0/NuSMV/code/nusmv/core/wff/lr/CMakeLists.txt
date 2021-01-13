set(SRCS
  LogicRecognizerBase.c
  LogicRecognizerCore.c
  MasterLogicRecognizer.c
)

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  LogicRecognizerBase.h
  LogicRecognizerCore.h
  MasterLogicRecognizer.h
  LogicRecognizerBase_private.h
  LogicRecognizerCore_private.h
  MasterLogicRecognizer_private.h
  )
