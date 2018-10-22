
#include "nusmv/core/utils/NodeList.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/dd/dd.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/parser/ord/ParserOrd.h"
#include "nusmv/core/compile/compile.h"

FILE *errstream;
FILE *outstream;
FILE *nusmv_stdin;
FILE *nusmv_historyFile;
FILE *nusmv_stdpipe;
DDMgr_ptr dd_manager = (DDMgr_ptr )NULL;

FILE* def_errstream;
FILE* def_outstream;

options_ptr options = (opts_ptr)NULL;
cmp_struct_ptr cmps = (cmp_struct_ptr)NULL;


void init_globals()
{
  nusmv_stdin = stdin;
  outstream = stdout;
  errstream = stderr;
}

int main()
{
  node_pkg_init();
  init_string();
  init_globals();

  test_ParserOrd();

  quit_string();
  node_pkg_quit(); 
  return 0;
}
