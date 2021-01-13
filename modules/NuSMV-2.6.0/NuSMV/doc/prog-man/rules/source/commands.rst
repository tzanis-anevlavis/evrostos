=================================
Documenting a NuSMV shell command
=================================

.. highlight:: c

Here is a significative example of how a NuSMV shell commands
documentation should look like.

Part of file ``NuSMV2/NuSMV/shell/bmc/bmcCmd.h``: ::

    /*!
      \command{bmc_inc_simulate} Incrementally generates a trace of the model
      performing a given number of steps.

      \command_args{\[-h\] \[-p | -v\] \[-r\]
      [\[-c "constraints"\] | \[-t "constraints"\] ] \[-k steps\]}

      bmc_inc_simulate performs incremental simulation
      of the model. If no length is specified with <i>-k</i> command
      parameter, then the number of steps of simulation to perform is
      taken from the value stored in the environment variable
      <i>bmc_length</i>.<BR>

      \command_opts{

        \opt{p, Prints current generated trace (only those variables
                whose value changed from the previous state)}

        \opt{v, Verbosely prints current generated trace (changed and
                 unchanged state variables)}

        \opt{r, Picks a state from a set of possible future states in
                a random way.}

        \opt{i, Enters simulation's interactive mode.}

        \opt{a, Displays all the state variables (changed and
                unchanged) in the interactive session}

        \opt{c "constraints", Performs a simulation in which
             computation is restricted to states satisfying those
             <tt>constraints</tt>. The desired sequence of states
             could not exist if such constraints were too strong or it
             may happen that at some point of the simulation a future
             state satisfying those constraints doesn't exist: in that
             case a trace with a number of states less than
             <tt>steps</tt> trace is obtained. The expression cannot
             contain next operators\, and is automatically shifted by
             one state in order to constraint only the next steps}

        \opt{t "constraints", Performs a simulation in which
             computation is restricted to states satisfying those
             <tt>constraints</tt>. The desired sequence of states
             could not exist if such constraints were too strong or it
             may happen that at some point of the simulation a future
             state satisfying those constraints doesn't exist: in that
             case a trace with a number of states less than
             <tt>steps</tt> trace is obtained.  The expression can
             contain next operators\, and is NOT automatically shifted
             by one state as done with option -c}

        \opt{k steps, Maximum length of the path according to the
            constraints.  The length of a trace could contain less
            than <tt>steps</tt> states: this is the case in which
            simulation stops in an intermediate step because it may
            not exist any future state satisfying those constraints.
            </dl>}
      }

      \se None
    */
    int Bmc_CommandBmcSimulate (NuSMVEnv_ptr env, int argc, char** argv);

Using the proper documentation tags for documenting commands results
in a better organized an better looking documentation: Each command
properly documented ends up in the **Commands** related page, and is
therefore more readable and easier to find

``Generated documentation of the example above``

.. image:: command.png


``The Commands page``

.. image:: commands.png
