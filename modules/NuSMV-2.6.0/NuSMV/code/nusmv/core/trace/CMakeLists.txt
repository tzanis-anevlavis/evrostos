set(SRCS
  pkg_trace.c
  Trace.c
  traceEval.c
  TraceLabel.c
  TraceMgr.c
  TraceOpt.c
  Trace_private.c
  traceTest.c
  traceUtils.c
  TraceXml.c
  
  eval/BaseEvaluator.c
  
  exec/BaseTraceExecutor.c
  exec/BDDCompleteTraceExecutor.c
  exec/BDDPartialTraceExecutor.c
  exec/CompleteTraceExecutor.c
  exec/PartialTraceExecutor.c
  exec/SATCompleteTraceExecutor.c
  exec/SATPartialTraceExecutor.c
  exec/traceExec.c
  
  loaders/TraceLoader.c
  
  plugins/TraceCompact.c
  plugins/TraceExplainer.c
  plugins/TracePlugin.c
  plugins/TraceTable.c
  plugins/TraceXmlDumper.c
  plugins/TraceEmpty.c
  )

if(HAVE_LIBXML2)
    set(SRCS
        ${SRCS}
        loaders/TraceXmlLoader.c
        )
endif()

nusmv_add_pkg_lib(${SRCS})

nusmv_add_pkg_headers(
  pkg_trace.h
  TraceLabel.h
  TraceXml.h
  TraceMgr.h
  TraceOpt.h
  Trace.h
  Trace_private.h

  plugins/TraceExplainer.h
  plugins/TraceTable.h
  plugins/TraceExplainer_private.h
  plugins/TraceTable_private.h
  plugins/TraceXmlDumper.h
  plugins/TraceXmlDumper_private.h
  plugins/TracePlugin.h
  plugins/TracePlugin_private.h
  plugins/TraceCompact.h
  plugins/TraceCompact_private.h

  exec/traceExec.h
  exec/BaseTraceExecutor.h
  exec/BaseTraceExecutor_private.h
  exec/CompleteTraceExecutor.h
  exec/CompleteTraceExecutor_private.h
  exec/PartialTraceExecutor.h
  exec/PartialTraceExecutor_private.h
  exec/BDDCompleteTraceExecutor.h
  exec/BDDCompleteTraceExecutor_private.h
  exec/BDDPartialTraceExecutor.h
  exec/BDDPartialTraceExecutor_private.h
  exec/SATCompleteTraceExecutor.h
  exec/SATCompleteTraceExecutor_private.h
  exec/SATPartialTraceExecutor.h
  exec/SATPartialTraceExecutor_private.h

  eval/BaseEvaluator.h
  eval/BaseEvaluator_private.h

  loaders/TraceLoader.h
  loaders/TraceLoader_private.h
  loaders/TraceXmlLoader.h
  loaders/TraceXmlLoader_private.h
  )
