===========================
Documenting a NuSMV package
===========================

With the introdution of the doxygen documentation generation, it is
now possible to write proper top-level documentation for *NuSMV* and
*NuSMV* addons packages.

Packages documentation can be placed in any directory that is under
the doxygen documentation path (see the doxygen configuration file for
details), but **must** have the `.doxy` extension.

One single `.doxy` documentation file can contain documentation of one
or more packages and sub-packages, but it is mandatory to specify the
belonging package directory with the `\\dir <path>` command.

An example for the node package and one of it's sub-packages would
look like

File ``ESTools/NuSMV/doc/doxygen/packages/node/node.doxy``: ::

    /*!
      \dir nusmv/core/node
      \brief This is the node package

      This package contains a lot of nodes..
    */

    /*!
      \dir nusmv/core/node/printers

      This sub-package contains a lot of node printers..
    */

However, these can be split-up onto two files:

File ``ESTools/NuSMV/doc/doxygen/packages/node/node.doxy``: ::

    /*!
      \dir nusmv/core/node
      \brief This is the node package

      This package contains a lot of nodes..
    */

File ``ESTools/NuSMV/doc/doxygen/packages/node/printers/printers.doxy``: ::

    /*!
      \dir nusmv/core/node/printers

      This sub-package contains a lot of node printers..
    */


---------------
TO BE DISCUSSED
---------------

Package documentation files can be placed everywhere in the source
tree. However, I would suggest using one of the two following rules:

1. Place them within an ad-hoc directory, as listed below:

+--------+--------------------------------------+
|  Code  | Docs directory                       |
+========+======================================+
| NuSMV  | ESTools/NuSMV/doc/doxygen/packages   |
+--------+--------------------------------------+
| addons | ESTools/addons/doc/doxygen/packages  |
+--------+--------------------------------------+

2. Place them directly in the package directory:

+--------+--------------------------------------+
|  Pkg   | Docs directory                       |
+========+======================================+
|  node  | ESTools/NuSMV/code/nusmv/core/node   |
+--------+--------------------------------------+
| simp   | ESTools/addons/src/addons/simp       |
+--------+--------------------------------------+
