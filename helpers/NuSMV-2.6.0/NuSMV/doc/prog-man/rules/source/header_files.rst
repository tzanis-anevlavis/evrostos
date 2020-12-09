===============================
Documenting a NuSMV header file
===============================

.. highlight:: c

Here is a significative example of how any NuSMV header file should
look like, whether it declares a pseudo-class or it contains normal
code::

    /* ---------------------------------------------------------------------------

      This file is part of NuSMV version 2. Copyright (C) 2014 by
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
      \brief Provides functionalities for foo

      This is the full description. It can go multiline. It supports
      <i>HTML</i> and `Markdown` (From doxygen >= 1.8). I would
      suggest using the `Markdown` format, since more readable and
      easier to learn.

      Provides functionalities for foo, which are here described in a
      detailed way.

      If Foo is a NuSMV pseudo-class, describe the class
      functionalities here.

      \todo: Support for foo2 is missing

    */

    #ifndef __NUSMV_CORE_UTILS_FOO_H__
    #define __NUSMV_CORE_UTILS_FOO_H__

    /*!
      \struct Foo
      \brief The Foo structure does foo

      Foo struct long description. Specifically for structs (this is the
      only case), the use of the \struct command is suggested for better
      documentation generation.
    */
    typedef struct Foo_TAG* Foo_ptr;

    /*!
      \brief Checks the given Foo instance

      Checks the given Foo instance. Check only checks whether the given
      value is NULL or not.
    */
    #define FOO_CHECK_INSTANCE(x)  \
            (nusmv_assert(FOO(x) != FOO(NULL)))

    /*!
      \brief A very nice enumeration

      Long description here
    */
    typedef enum FooEnum_TAG {
            VAL1 = 1, /*!< Docs for VAL1 */
            VAL2 = 2, /*!< Docs for VAL2 */
            VAL3 = 3 /*!< Docs for VAL3 */
    } FooEnumType;

    /*!
      \brief A function that does something with the enviroment and returns
        a Foo instance

      A longer description about the function that does something with the
      enviroment and returns a Foo instance.

      Call this function is this way (this will be shown as code
      snippet, since separated from the text with an empty line and
      indented by 4 spaces):

          Foo_ptr foo = Foo_do_something(env, 2, strings);

      Parameter strings is freed by the Foo_do_something function, therefore we
      add the <b>takes_mem</b> command

      The return value memory is handled internally, so it must not be
      freed by the caller. In this case, we add the <b>keeps_mem</b>
      command.

      \todo Missing description about Foo Fighters.

      \param env The environment
      \param param An integer parameter
      \param strings The input list of strings \takes_mem
      \return A Foo instance. \keeps_mem

      \todo Improve this documentation
      \sa Foo_do_something_2 (this is for See-Also)
      \se The given NuSMVEnv_ptr instance is changed (this is for Side-Effect)
    */
    Foo* Foo_do_something(NuSMVEnv_ptr env, int param, char** strings);

    #endif /* __NUSMV_CORE_UTILS_FOO_H__ */
    
Here is a significative example of how to rightly describe a method, it's mandatory to 
use the directive `\\methodof` in order to correctly associate "methods" to the corresponding classes in doxygen documentation. ::
    
    /*!
       \methodof ClassName 
       \brief Short description

       Longer description...
    */
     <type> ClassName_method_name(ClassName* self, ...);

 
