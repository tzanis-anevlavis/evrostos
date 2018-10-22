/* WARNING [MD] THIS FILE IS NOT COMPILED */
#error

#include "nusmv/core/utils/StreamMgr.h"
#include "nusmv/core/node/printers/MasterPrinter.h"
#include "nusmv/core/opt/opt.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/utils/ustring.h"
#include "nusmv/core/parser/psl/pslNode.h"
#include "nusmv/core/compile/compile.h"

#include <stdio.h>

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

extern DDMgr_ptr dd_manager;
extern cmp_struct_ptr cmps;

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/

static void psl_init()
{
  init_memory();
    
  outstream     = stdout;
  errstream     = stderr;

  init_string();
  Opt_Pkg_init();
  node_pkg_init(); 

  dd_manager = init_dd_package();
  init_the_node();

  Compile_init();
}


static void psl_quit()
{

  Compile_quit();
  quit_dd_package(dd_manager);  
  node_pkg_quit();
  quit_string();

  if (outstream != stdout) fclose(outstream);  
  if (errstream != stderr) fclose(errstream);
}

extern PslNode_ptr psl_parsed_tree;

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DOEXPAN 1

/*!
  \brief \todo Missing synopsis

  \todo Missing description
*/
#define DOCONV  1

int main() 
{
  int res; 

  psl_init();
  res = psl_yyparse();

  if (res == 0) {

    StreamMgr_print_output(streams,  "Parsed expression\n");

    StreamMgr_nprint_output(streams, wffprint, "%N", psl_parsed_tree);
    
    StreamMgr_print_output(streams,  "\n");

    if (PslNode_is_obe(psl_parsed_tree)) {
      StreamMgr_print_output(streams, "Parsed an OBE (CTL) expression\n");
#if DOEXPAN
      StreamMgr_print_output(streams, "The expanded OBE expression is\n");
      print_node(wffprint, outstream, 
		 PslNode_pslobe2ctl(psl_parsed_tree, PSL2PSL));
      StreamMgr_print_output(streams, "\n\n");    
#endif

#if DOCONV
      StreamMgr_print_output(streams, "The SMV compatible CTL expression is\n");
      print_node(wffprint, outstream, 
		 PslNode_pslobe2ctl(psl_parsed_tree, PSL2SMV));
      StreamMgr_print_output(streams, "\n\n");
#endif 
    }
    else if (PslNode_is_handled_psl(psl_parsed_tree)) {
      PslNode_ptr expr=psl_parsed_tree;

#if 1
      if (!PslNode_is_ltl(expr)) {
        PslNode_ptr m;
        
	/* handled sere */
	m = PslNode_remove_sere(expr);
	expr=m;

	StreamMgr_print_output(streams, "The translated SERE expression is\n");
	StreamMgr_nprint_output(streams, wffprint, "%N", expr);
	StreamMgr_print_output(streams, "\n\n");

      }
#endif
      StreamMgr_print_output(streams,  "Parsed a LTL expression\n");
#if DOEXPAN
      StreamMgr_print_output(streams, "The expanded LTL expression is\n");
      print_node(wffprint, outstream, 
		 PslNode_pslltl2ltl(expr, PSL2PSL));
      StreamMgr_print_output(streams, "\n\n");
#endif
      
#if DOCONV
      StreamMgr_print_output(streams, "The SMV compatible LTL expression is\n");
      print_node(wffprint, outstream, 
		 PslNode_pslltl2ltl(expr, PSL2SMV));
      StreamMgr_print_output(streams, "\n\n");
#endif 
    }
    else {
      StreamMgr_print_output(streams, "Parsed a NON LTL/CTL expression\n");
    }
  }
  else {
    StreamMgr_print_output(streams, "Parsing error\n");
  }

  StreamMgr_print_output(streams, "\n");
  
  psl_quit();
  return res;
}
