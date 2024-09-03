curry-funcinfo
==============

A simple program to show analysis information about a function defined
in a module of some package.

To start the tool:

    > curry-funcinfo <module name> <function name>

If the tool is used inside a package, it should be invoked as

    > cypm exec curry-funcinfo <module name> <function name>

so that functions defined in imported packages can be found.
