/* ---------------------------------------------------------------------------


  This file is part of the ``trace'' package of NuSMV version 2.
  Copyright (C) 2003 by FBK-irst.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA.

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>. 

-----------------------------------------------------------------------------*/

/*!
  \author Ashutosh Trivedi
  \brief Routines related to functionality related to a node of a trace.

  This file contains the definition of the \"TraceLabel\" class.

*/


#include "nusmv/core/node/NodeMgr.h"
#include "nusmv/core/trace/TraceLabel.h"
#include "nusmv/core/node/node.h"
#include "nusmv/core/parser/symbols.h"
#include "nusmv/core/parser/parser.h"

#include "nusmv/core/utils/portability.h" /* for errno */
#include <limits.h> /* for INT_MAX */

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Definition of external functions                                          */
/*---------------------------------------------------------------------------*/

TraceLabel TraceLabel_create(NodeMgr_ptr nodemgr,
                             int trace_id, int state_id)
{
  TraceLabel self;

  self = find_node(nodemgr, DOT, find_node(nodemgr, NUMBER, NODE_FROM_INT(trace_id), Nil),
                   find_node(nodemgr, NUMBER, NODE_FROM_INT(state_id), Nil));
  return self;
}

TraceLabel TraceLabel_create_from_string(NodeMgr_ptr nodemgr, const char* str)
{
  char* startptr;
  char* endptr;
  long traceno, stateno;

  /* Start reading the string */
  startptr = (char*)str;

  /* First number must be a positive integer */
  errno = 0;
  traceno = strtol(str, &endptr, 10);

  if((startptr == endptr) || (errno == ERANGE) ||
     (errno == EINVAL) || (traceno < 0) || (traceno > INT_MAX)) {
    /*
      No chars read or error in reading or negative number or
      number too big
    */
    return TRACE_LABEL_INVALID;
  }
  startptr = endptr;

  /* We can have some spaces */
  while(' ' == *startptr) {
    startptr++;
  }

  /* Then we have a '.' char */
  if ('.' == *startptr) {
    startptr++;
  }
  else {
    /* We have something which is not a '.' */
    return TRACE_LABEL_INVALID;
  }

  /* And then the state number */
  errno = 0;
  stateno = strtol(startptr, &endptr, 10);

  if((startptr == endptr) || (errno == ERANGE) ||
     (errno == EINVAL) || (stateno > INT_MAX) || (stateno < INT_MIN)) {
    /* No chars read or error in reading or invalid int */
    return TRACE_LABEL_INVALID;
  }
  startptr = endptr;

  /* Skip final spaces */
  while(' ' == *startptr) {
    startptr++;
  }

  if ('\0' == *startptr) {
    return TraceLabel_create(nodemgr, (int)(traceno - 1), (int)(stateno - 1));
  }
  else {
    /* Something unexpected at the end of the file */
    return TRACE_LABEL_INVALID;
  }
}

int TraceLabel_get_trace(TraceLabel self)
{
  int result = NODE_TO_INT(car(car((node_ptr)self)));

  return result;
}

int TraceLabel_get_state(TraceLabel self)
{
  int result = NODE_TO_INT(car(cdr((node_ptr)self)));

  return result;
}

