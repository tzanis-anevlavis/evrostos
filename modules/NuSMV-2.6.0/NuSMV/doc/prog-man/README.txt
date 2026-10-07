########################################
## NuSMV documentation with doxygen.  ##
## Author: Alessandro Mariotti        ##
########################################

For generating the NuSMV doxygen documentation rules and examples,
make sure you have sphinx-doc installed and run "make" or "./make.bat"
from the rules/ directory

For generating the documentation, run from top-level directory
(trunk/NuSMV2/NuSMV) the following:

doxygen doc/doxygen/doxygen_nusmv.conf

You will find the generated documentation in the
doc/doxygen/build/html and doc/doxygen/build/latex directories

Tested with doxygen 1.8.7

Read the RULES document for documentation howto and rules.

POSSIBLE CHANGES:

- Package files (now listed in the doc/doxygen/packages/ directory)
  can be placed anywhere in the code tree without any change. Possibly
  we want them in the corresponding directory:

  e.g. doc/doxygen/packages/node.doxy can be placed in
    code/nusmv/core/node/
