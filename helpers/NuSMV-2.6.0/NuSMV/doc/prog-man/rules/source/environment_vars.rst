========================================
Documenting a NuSMV environment variable
========================================

.. highlight:: c

Here is a significative example of how a NuSMV environment variables
documentation should look like. Place the environment documentation
above the definition of the name of the variable:

Part of file ``NuSMV2/NuSMV/core/opt/opt.h``: ::

    /*!
      \env_var{input_order_file} The input order file

      Longer description

    */
    #define INPUT_ORDER_FILE  "input_order_file"

Using the proper documentation tags for documenting environment
variables results in a better organized an better looking
documentation: Each environment variable properly documented ends up
in the **Environment variables** related page, and is therefore more
readable and easier to find (See screenshot)

.. image:: env.png
