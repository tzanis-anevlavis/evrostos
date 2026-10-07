set(contrib_files
  README
  iscas89-2smv
  kiss2-2smv
  smv2table.awk
  smv-mode.el
  order_bits.pl
  nusmv-mode.el
  NuSMV.m4
  emacs-gdb-macros.el
  remove_string.pl
  )

install(FILES ${contrib_files} DESTINATION share/nusmv/contrib)

