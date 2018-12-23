# Evrostos: The rLTL Verifier
## Installation and instructions guide.
This file contains instructions on how to install and call _Evrostos: The rLTL Verifier_. Instructions on how to replicate the case studies of the paper can be found at replicationInstructions-rLTL.pdf (for the rLTL specifications) and at replicationInstructions-LTL.pdf (for the LTL specifications). The reports of the simulations of the paper are found in the ./examples/results_paper folder.

All instructions and folders are relative to the folder you have extracted this archive to.

Evrostos is publicly available online at this [github repository](https://github.com/janis10/evrostos).

### Contents
* System Requirements
* Folder Contents
* Installation Guide
    1. NuSMV Installation Instructions
    2. rLTL2LTL Instructions
    3. Evrostos: The rLTL Verifier
* Calling Evrostos
* The right variables for a specification with Evrostos

### System Requirements
1. Operating systems: macOS or Linux.
2. CMake version 2.8 or greater.
3. Java™ Platform, Standard Edition Development Kit (JDK™) (version 10.0.2 or later).
4. Python 2.
5. You will also need:
    * An ANSI C compiler (gcc will do, as will several versions of cc)
    * An ANSI C++ compiler (g++ will do)
    * GNU Flex version 2.5 or greater
    * GNU Bison version 1.22 or greater
    * GNU Readline
    * GNU make utility version 3.74 or greater
    * GNU patch utility
    * GNU tar and gzip utilities
    * Library libxml2

**NOTE:** There are known issues with compiling NuSMV/CUDD with with the GNU compiler collection version >= 6 that prevent NuSVM from compiling properly. We successfully ran simulations with compiler version 4.2.1 for macOS and 5.3.1 for Ubuntu.


### Folder Contents
The evrostos-master folder includes the following folders and files.

1. NuSMV-2.6.0
2. rltl2ltl-master
3. examples
4. evrostosSource.c
5. routines.c
6. LTLmodelcheck.c
7. routines4ltl.c
8. README.md
9. Makefile

### Installation Guide
To install Evrostos one can either use the Makefile included or install it manually. 
For the makefile option just invoke `make` while on the main folder where the Makefile is located. We suggest this option.
The instructions to install the components manually are as follows.

### 1. NuSMV Installation Instructions:
1. Create a folder named `build` inside `./NuSMV-2.6.0/NuSMV/`.  
Then change your directory to:
`./NuSMV-2.6.0/NuSMV/build`
2. Configure by invoking “cmake”: 
`cmake ..`
This will configure NuSMV.
NOTICE: now in all “Makefiles” the default directory will be:
`./NuSMV-2.6.0/NuSMV/---`
3. Compile NuSMV:
While in the following directory,
`./NuSMV-2.6.0/NuSMV/build` 
compile by typing make.
This will build an executable “NuSMV” in the 
`./NuSMV-2.6.0/NuSMV/build/bin directory`!

Note: when Evrostos is calling NuSMV the following options are used by default:

* -coi        : Cone of influence. Depending on the properties to check, this can be very effective.
* -df     : Disable computation of reachable states.
* -dcx        : Disable generation of counterexample traces.
* -dynamic    : Enable the dynamic variable reordering

**DISCLAIMER**:
For performance and aesthetic reasons all the original initialization messages of NuSMV 2.6.0
have been suppressed. These small modifications are found in the following files with the comment “J-Edit”:

1. File: ./NuSMV-2.6.0/NuSMV/code/nusmv/core/ltl/ltl.c:
    * Includes at line 79;
    * Lines 413-458.
2. File: ./NuSMV-2.6.0/NuSMV/code/nusmv/core/cinit/cinitVers.c:
    * Lines 160 to 265: Commented-out “fprintf”s to suppress output when initializing NuSMV.

Hereby, we **do not claim as of our own** any of the code or functionality of NuSMV and use it only as a component of our tool by the rights of open source distributed software.
For more information on NuSMV please visit [http://nusmv.fbk.eu](nusmv.fbk.eu).

### 2. rLTL2LTL Instructions:
You will need to have installed Java™ Platform, Standard Edition Development Kit (JDK™) (version 10 or later).
rLTL2LTL translator comes precompiled and no further action is needed.

### 3. Evrostos: The rLTL Verifier:
In the `evrostos-master` directory, compile Evrostos using the following command:  
`gcc -w evrostosSource.c -o evrostos`  
this will create the executable file “evrostos”.

### Calling Evrostos
Running Evrostos from the terminal is care-free and straightforward.  
While in the directory
`./evrostos-master/`
where the executable file “evrostos” can be found, just type on the terminal:  
`./evrostos -options`  
where -options can be:

* -h: help - displays help message
* -i: rLTL specification input is given via keyboard
* -I: rLTL specification input is given via .txt file

We recommend using option -I which allows checking multiple rLTL formulas for the same model with only one run. Example of running Evrostos:  
After running the command
`./evrostos -I`,
the terminal looks as follows:  
`Enter the rLTL specification input file name (.txt):`  
_Enter filename (if it is in the evrostos-master folder) or path to filename of the .txt file used as input._  
`Enter the model file name (.smv):`  
_Enter filename (if it is in the evrostos-master folder) or path to filename of the .smv file used as input._  
`Enter file name (.txt) for the report:`  
_Enter filename of the .txt report to be created (it will be created in the evrostos-master folder)._

### The right variables for a specification with Evrostos:
Evrostos uses NuSMV 2.6.0 to perform in a bitwise manner the rLTL verification.

In the same manner as when using NuSMV you will have to use the variables of the .smv model file at hand.
You SHOULD NOT write the rLTL specifications directly to the .smv file.
Evrostos will handle this internally, just make sure the specification used as input adheres to the variables of your model !

Evrostos handles ONLY boolean variables. 
For example, to check an assignment of the form “rG (p = 5)”, 
where p is an integer variable of the model used, 
define inside the .smv file a new boolean variable, e.g., “q := p = 5”.
Then the rLTL formula to be model checked is “rG q”.
To see how to define new variables refer to NuSMV user manual or tutorial at [http://nusmv.fbk.eu](http://nusmv.fbk.eu).

### References:
Tzanis Anevlavis, Daniel Neider, Matthew Philippe and Paulo Tabuada.
"Evrostos: The rLTL Verifier".
To appear in the 22nd ACM International Conference on Hybrid Systems: Computation and Control (HSCC 2019). 


A. Cimatti, E. Clarke, E. Giunchiglia, F. Giunchiglia, M. Pistore, M. Roveri, R. Sebastiani, and A. Tacchella.
"NuSMV 2: An OpenSource Tool for Symbolic Model Checking".
In Proc. CAV'02, LNCS. Springer Verlag, 2002.
