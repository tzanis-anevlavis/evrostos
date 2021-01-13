rLTL2LTL
========
rLTL2LTL is a tool to translate rLTL formulas into LTL formulas


Getting Started
---------------
Download or clone the sources and make sure you satisfy the prerequisites listed
below.


Prerequisites
-------------
RLTL2LTL has the following dependencies:

* **JavaCC** (https://javacc.org/)
* the Apache **ANT** build tool (http://ant.apache.org/)

If necessary, please consult the corresponding
websites for further information of how to download and/or install them.


Compilation
-----------
The sources can easily be compiled using ANT. Simply perform the following two
steps:

1.  Change into the directory containing the file build.xml and run the command
    `ant`.

    By default, JavaCC is assumed to be located in /usr/share/java/. If JavaCC
    is installed in another directory (e.g., if you are not running Linux), set
    the environment variable JAVACC_HOME to the appropriate directory JavaCC
    prior to running ant.

    After the sources have been compiled successfully, the class files are
    located in ./build.

2. Run the command `ant jar` to create a .jar file called rltl2ltl.jar, which is
   placed in the current directory.

    To generate Javadoc documentation of the sources, run
    `ant generate_javadoc`.

    To clean all generated files, run `ant clean`.


Running rLTL2LTL
----------------
To run rLTL2LTL, execute `java -jar rltl2ltl.jar` with the desired options. Use
the option `-h` to display further information.
