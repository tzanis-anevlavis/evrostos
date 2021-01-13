
HEAD_COMMENT = \
"""
/* ---------------------------------------------------------------------------

  This file is part of the ``%(pkgname)s'' package of NuSMV version 2.
  Copyright (C) 2014 by FBK-irst.

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
   \\author %(author)s
   \\brief %(synopsis)s

   %(description)s

   \\sa %(seealso)s
*/
"""

HEAD_COMMENT_ESMC = \
"""

/* ---------------------------------------------------------------------------

%%COPYRIGHT%%

-----------------------------------------------------------------------------*/

/*!
   \\author %(author)s
   \\brief %(synopsis)s

   %(description)s

   \\sa %(seealso)s
*/
"""

# ----------------------------------------------------------------------
HEADER_START = \
"""
#ifndef __%(upcase_filename)s__
#define __%(upcase_filename)s__

"""

# ----------------------------------------------------------------------
HEADER_END = \
"""

#endif /* __%(upcase_filename)s__ */
"""

# ----------------------------------------------------------------------
INCLUDE = \
"""#include %(inc_file)s"""


# ----------------------------------------------------------------------
IFC_SECTIONS = \
"""
/*!
  \\struct %(ClassName)s
  \\brief Definition of the public accessor for class %(ClassName)s
*/
typedef struct %(ClassName)s_TAG*  %(ClassName)s_ptr;


/*!
  \\brief To cast and check instances of class %(ClassName)s

  These macros must be used respectively to cast and to check
  instances of class %(ClassName)s

*/
#define %(CLASS_NAME)s(self) \\
         ((%(ClassName)s_ptr) self)

#define %(CLASS_NAME)s_CHECK_INSTANCE(self) \\
         (nusmv_assert(%(CLASS_NAME)s(self) != %(CLASS_NAME)s(NULL)))




/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \\methodof %(ClassName)s
  \\brief The %(ClassName)s class constructor

  The %(ClassName)s class constructor

  \\sa %(ClassName)s_destroy
*/
%(ClassName)s_ptr %(ClassName)s_create (void);


/*!
  \\methodof %(ClassName)s
  \\brief The %(ClassName)s class destructor

  The %(ClassName)s class destructor

  \\sa %(ClassName)s_create
*/
void %(ClassName)s_destroy (%(ClassName)s_ptr self);


"""

PROTECTED_METHODS_DECL = \
"""
/* ---------------------------------------------------------------------- */
/* Private methods to be used by derivated and friend classes only         */
/* ---------------------------------------------------------------------- */
/*!
  \\methodof %(ClassName)s
  \\brief The %(ClassName)s class private initializer

  The %(ClassName)s class private initializer

  \\sa %(ClassName)s_create
*/
void %(class_name)s_init (%(ClassName)s_ptr self);


/*!
  \\methodof %(ClassName)s 
  \\brief The %(ClassName)s class private deinitializer

  The %(ClassName)s class private deinitializer

  \\sa %(ClassName)s_destroy
*/
void %(class_name)s_deinit (%(ClassName)s_ptr self);
"""

PRIVATE_METHODS_DECL = \
"""
static void %(class_name)s_finalize (Object_ptr object, void* dummy);
"""


PRIVATE_METHODS_DECL_NO_INHERITANCE = \
"""
static void %(class_name)s_init (%(ClassName)s_ptr self);
static void %(class_name)s_deinit (%(ClassName)s_ptr self);
"""


#----------------------------------------------------------------------
DEF_SECTIONS = \
"""
/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
%(class_def)s

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/



/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/
%(static_meth_decl)s

/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/
%(public_meth_def)s

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/
%(protected_meth_def)s

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
%(private_meth_def)s



"""


CLASS_DEFINITION_NO_INHERITANCE = \
"""
/*!
  \\brief %(ClassName)s class definition

  %(ClassDescription)s
*/
typedef struct %(ClassName)s_TAG
{
  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


} %(ClassName)s;

"""


CLASS_DEFINITION_INHERITANCE = \
"""
/*!
  \\brief %(ClassName)s class definition derived from
         class %(BaseClassName)s

  %(ClassDescription)s

  \\sa Base class %(BaseClassName)s
*/
typedef struct %(ClassName)s_TAG
{
  /* this MUST stay on the top */
  INHERITS_FROM(%(BaseClassName)s);

  /* -------------------------------------------------- */
  /*                  Private members                   */
  /* -------------------------------------------------- */


  /* -------------------------------------------------- */
  /*                  Virtual methods                   */
  /* -------------------------------------------------- */

} %(ClassName)s;

"""



CLASS_PUBLIC_METHODS_NO_INHERITANCE = \
"""
%(ClassName)s_ptr %(ClassName)s_create(void)
{
  %(ClassName)s_ptr self = ALLOC(%(ClassName)s, 1);
  %(CLASS_NAME)s_CHECK_INSTANCE(self);

  %(class_name)s_init(self);
  return self;
}

void %(ClassName)s_destroy(%(ClassName)s_ptr self)
{
  %(CLASS_NAME)s_CHECK_INSTANCE(self);

  %(class_name)s_deinit(self);
  FREE(self);
}
"""

CLASS_PRIVATE_METHODS_NO_INHERITANCE = \
"""
/*!
  \\brief The %(ClassName)s class private initializer

  The %(ClassName)s class private initializer

  \\sa %(ClassName)s_create
*/
static void %(class_name)s_init(%(ClassName)s_ptr self)
{
  /* members initialization */

}


/*!
  \\brief The %(ClassName)s class private deinitializer

  The %(ClassName)s class private deinitializer

  \\sa %(ClassName)s_destroy
*/
static void %(class_name)s_deinit(%(ClassName)s_ptr self)
{
  /* members deinitialization */

}
"""


CLASS_PUBLIC_METHODS = \
"""
%(ClassName)s_ptr %(ClassName)s_create(void)
{
  %(ClassName)s_ptr self = ALLOC(%(ClassName)s, 1);
  %(CLASS_NAME)s_CHECK_INSTANCE(self);

  %(class_name)s_init(self);
  return self;
}


void %(ClassName)s_destroy(%(ClassName)s_ptr self)
{
  %(CLASS_NAME)s_CHECK_INSTANCE(self);

  Object_destroy(OBJECT(self), NULL);
}
"""

CLASS_PROTECTED_METHODS = \
"""
void %(class_name)s_init(%(ClassName)s_ptr self)
{
  /* base class initialization */
  %(base_class_name)s_init(%(BASE_CLASS_NAME)s(self));

  /* members initialization */

  /* virtual methods settings */
  OVERRIDE(Object, finalize) = %(class_name)s_finalize;

  /* for example, to override a base class' virtual method: */
  /*OVERRIDE(%(BaseClassName)s, virtual_method) = %(class_name)s_virtual_method;*/
}


void %(class_name)s_deinit(%(ClassName)s_ptr self)
{
  /* members deinitialization */


  /* base class deinitialization */
  %(base_class_name)s_deinit(%(BASE_CLASS_NAME)s(self));
}
"""

CLASS_PRIVATE_METHODS = \
"""
/*!
  \\brief The %(ClassName)s class virtual finalizer

  Called by the class destructor
*/
static void %(class_name)s_finalize(Object_ptr object, void* dummy)
{
  %(ClassName)s_ptr self = %(CLASS_NAME)s(object);

  %(class_name)s_deinit(self);
  FREE(self);
}
"""
