# Evrostos: The rLTL Verifier
This is _Evrostos_, a tool that Efficiently Verifies RObuSTness Of Specifications! Evrostos is a model-checking tool for Robust Linear-time Temporal Logic (rLTL). The logic rLTL was crafted to embed the notion of _robustness_ into LTL. The main advantages of rLTL are: 
1. it expresses a notion of robustness in _reactive specifications_ inspired by control theory ("small" violations of the assumption, should lead to, at most, "small" violations of the guarantee), whereas LTL cannot.
2. rLTL is a 5-valued logic, which provides more fine-grained information whenever a specification is violated, which corresponds to different degrees of violation (e.g., violation finitely many times, infinitely many times, or at every step).

Therefore, rLTL verification provides much more insight to the designer than classical LTL verification. For more details, see _Evrostos: The rLTL Verifier_ https://doi.org/10.1145/3302504.3311812 .

Evrostos is easy to use, and works with your favorite LTL model-checker, bridging the gap between classical LTL model-checking, and rLTL model-checking in an effortless way. That is, one has *to only refine the syntax of the LTL specification accordingly, to obtain the rLTL version* (for example replace the always operator `G` with the robust always operator `rG`). 
Currently, as an underlying LTL model-checker, Evrostos is compatible with NuSMV 2.6.0 (14 October 2015), and SPIN Version 6.5.1 (31 July 2020).

Evrostos is publicly available online at this [github repository](https://github.com/janis10/evrostos).
    
#### Citations:
If you used this tool for efficiently verifying robustness of your specifications please cite as:
```latex
@inproceedings{AT2019Evrostos,
 author = {Anevlavis, Tzanis and Neider, Daniel and Phillipe, Matthew and Tabuada, Paulo},
 title = {Evrostos: The rLTL Verifier},
 booktitle = {Proceedings of the 22Nd ACM International Conference on Hybrid Systems: Computation and Control},
 series = {HSCC '19},
 year = {2019},
 isbn = {978-1-4503-6282-5},
 location = {Montreal, Quebec, Canada},
 pages = {218--223},
 numpages = {6},
 url = {http://doi.acm.org/10.1145/3302504.3311812},
 doi = {10.1145/3302504.3311812},
 acmid = {3311812},
 publisher = {ACM},
 address = {New York, NY, USA},
 keywords = {formal methods, model checking, robustness, temporal logic},
} 
```

#### For more rLTL reading..
##### Genesis of Robust Linear-time Temporal Logic (rLTL):
P. Tabuada and D. Neider. Robust linear temporal logic. In Proceedings of the 25th Conference on Computer Science Logic, pages 10:1–10:21, Marseille, France, 2016.
ArXiv version (for more details!): https://arxiv.org/abs/1510.08970

##### About an efficient fragment of rLTL and a gentler first contact:
T. Anevlavis, M. Philippe, D. Neider, and P. Tabuada. 2018. Verifying rLTL formulas: now faster than ever before!. In 2018 IEEE Conference on Decision and Control (CDC). 1556–1561. https://doi.org/10.1109/CDC.2018.8619014

## Installation and instructions guide.
This section contains instructions on how to install and call _Evrostos: The rLTL Verifier_. 

All instructions and folders are relative to the folder you have extracted this archive to.

### Contents
* System Requirements
* Building Evrostos
* rLTL syntax with Evrostos
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


### Building Evrostos
To build Evrostos just invoke `make` while on the main folder where the Makefile is located. Our Makefile will conveniently build both NuSMV and SPIN from scratch, from their corresponding source code. 

**It is important to use the NuSMV and SPIN source code as found in our repository.** This is since their respective source codes have been slightly modified to allow communication with Evrostos. Any changes have not influenced functionality of these tools and have been made possible under their corresponding licences (LGPL v2.1 for NuSMV 2.6.0, and 3-clause BSD License for SPIN 6.5.1). For both NuSMV and SPIN the modifications can be found with by looking for the key "J-Edit" in the comments of the modified source code files. 

Finally, the rltl2ltl translator comes pre-compiled, although its source code can be found in the corresponding folder. 

For performance and aesthetic reasons all the original initialization messages of NuSMV 2.6.0 have been suppressed. These small modifications are found in the following files with the comment “J-Edit”:

1. File: ./helpers/NuSMV/NuSMV/code/nusmv/core/ltl/ltl.c:
    * Purpose: communicate with the cpp wrapper.
2. File: ./helpers/NuSMV/NuSMV/code/nusmv/core/cinit/cinitVers.c:
    * Purpose: commented-out “fprintf”s to suppress output when initializing NuSMV.
3. File: ./helpers/Spin/Src/pangen1.h:
    * Purpose: communicate with the cpp wrapper.

Hereby, we **do not claim as our own any of the code or functionality of NuSMV or SPIN**, and use it only as a component of our tool under their corresponding licenses as stated above. 
For more information on NuSMV please visit [http://nusmv.fbk.eu](nusmv.fbk.eu), and on SPIN please visit [https://github.com/nimble-code/Spin](https://github.com/nimble-code/Spin).

### rLTL syntax with Evrostos and the efficient rLTL fragment
The rLTL syntax when used with Evrostos is defined as follows:

* Grammar:
	* rltl ::= opd | ( rltl ) | rltl binop rltl | unop rltl

* Operands (opd):
	* 0000, 0001, 0011, 0111, 1111, user-defined names starting with a lower-case letter.

* Unary Operators (unop):
	* rG	(the temporal operator robust always)
	* rF	(the temporal operator robust eventually)
    * rX  (the temporal operator robust next)
	* ! 	(the boolean operator for robust negation)

* Binary Operators (binop):
	* rU 	(the temporal operator robust until)
	* rR 	(the temporal operator robust release) : notice that the LTL duality between until and release does not hold in rLTL.
	* &	(the operator for logical and)
	* |	(the operator for logical or)
	* =>	(the operator for robust implication)
   
[add latex description of the efficient rLTL fragment].


### Calling Evrostos
Running Evrostos from the terminal is care-free and straightforward.  
While in the directory
`./evrostos-master/`
where the executable file “evrostos” can be found, just type on the terminal:  
`./evrostos`.

After running the above command the terminal looks as follows:  
`Specify input file:`  
_Enter filename (if it is in the evrostos-master folder) or path to filename of the .txt file used as input._  

The input file is the only input that Evrostos needs, and contains the following information:

1. Modelchecker: select either SPIN or NuSMV in the next line.
2. Model name: the model name appears in the next line. Depending on the above choice, the extension of this file is either `.smv` or `.pml`.
3. rLTLspecs: N , where N is the number of rLTL specifications to be model-checked, and is followed in each next line by the exact rLTL formulae.
4. Flags: any admissible flags the model-checker of your choice supports appear in the next line. 

Two instructive examples of how the input file should look can be found in `examples/instructional_example`. 
We **really recommend** to play with the supported input files to increase familiarity. 

### The right variables for a specification with Evrostos:
In the same manner as when using NuSMV or SPIN you will have to use the variables of the .smv or .pmv model file at hand.
You **SHOULD NOT write the rLTL specifications directly to the model file (.smv or .pmv)**.
Evrostos will handle this internally, just make sure the specification used as input adheres to the variables of your model !

**Evrostos handles ONLY boolean variables.**
For example, to check an assignment of the form “rG (p=5)”, where p is an integer variable of the model used, 
define a new boolean variable, e.g., “q := (p=5)”, inside the .smv file, or “#define  q  p=5”, inside the .pml file.
Then the rLTL formula to be model-checked is “rG q”.
To see how to define new variables refer to respective manuals for NuSMV and SPIN. 

### Examples:
You can find all the necessary files to replicate the simulations in the Experimental Results section of the paper introducing Evrostos, *"Evrostos: The rLTL Verifier", HSCC 2019* in the folder `./examples/HSCC19`. Additional instructions on how to replicate each of these examples are also provided in that folder. 

### References:
Tzanis Anevlavis, Daniel Neider, Matthew Phillipe, and Paulo Tabuada. 2019. Evrostos: the rLTL verifier. In Proceedings of the 22nd ACM International Conference on Hybrid Systems: Computation and Control (HSCC '19). ACM, New York, NY, USA, 218-223. DOI: https://doi.org/10.1145/3302504.3311812

Alessandro Cimatti, Edmund M. Clarke, Enrico Giunchiglia, Fausto Giunchiglia, Marco Pistore, Marco Roveri, Roberto Sebastiani, and Armando Tacchella. 2002. NuSMV 2: An OpenSource Tool for Symbolic Model Checking. In Proceedings of the 14th International Conference on Computer Aided Verification (CAV ’02). Springer-Verlag, London, UK, UK, 359–364.

Gerard Holzmann. 2003. Spin model checker, the: primer and reference manual (First. ed.). Addison-Wesley Professional.
