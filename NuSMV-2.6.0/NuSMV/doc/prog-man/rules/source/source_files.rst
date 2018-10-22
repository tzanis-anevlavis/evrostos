===============================
Documenting a NuSMV source file
===============================

.. highlight:: c

The documentation rules for header files apply also for the source
files. Altough source file members documentation is optional, the
copyright header and the first doxygen entry, where author and
description of the file are described, are mandatory.

Currently, doxygen documentation within a source file is not exported
in the generated documentation. For exporting such documentation,
please put it in a *.doxy file, which will be automatically read by
doxygen.

Here is a significative example of how a NuSMV source file should look
like::

    /* ---------------------------------------------------------------------------

       This file is part of NuSMV version 2.  Copyright (C) 2014 by
       FBK-irst.

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
      \author Alessandro Mariotti
      \brief Implementation of Foo

      A long description about the Foo implementation
    */

    #include "nusmv/core/Foo.h"

    /*---------------------------------------------------------------------------*/
    /* Type declarations                                                         */
    /*---------------------------------------------------------------------------*/

    typedef struct Foo_TAG
    {
      INHERITS_FROM(EnvObject);

      NodeMgr_ptr nodes; /*!< Used for something about Foo */

    } Foo;

    /*---------------------------------------------------------------------------*/
    /* Static function prototypes                                                */
    /*---------------------------------------------------------------------------*/

    static int
    foo_do_something_new(const Foo_ptr foo);

    /*---------------------------------------------------------------------------*/
    /* Definition of exported functions                                          */
    /*---------------------------------------------------------------------------*/

    Foo* Foo_do_something(NuSMVEnv_ptr env, int param, char** strings)
    {
       /* Do a lot of stuff */
    }

    /*--------------------------------------------------------------------------*/
    /* Definition of static functions                                           */
    /*--------------------------------------------------------------------------*/

    /*!
      \brief Does something new with Foo

      \param foo the Foo instance
      \return an integer that has something to do with the given foo instance
    */
    static int foo_do_something_new(const Foo_ptr foo)
    {
            /* Do a lot of stuff */
    }
