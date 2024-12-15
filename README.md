cpm-query
=========

This package contains the implementation of a tool to query
analysis information about entities (e.g., function, types, typeclasses)
defined in a module of some Curry package.

The tool is based and, therefore, requires the executable `curry-info`
to manage various Curry analysis tools.

To start the tool:

    > cpm-query <module name> <function name>

or

    > cpm-query --type <module name> <type name>

or
    > cpm-query --typeclass <module name> <typeclass name>

Note that `cypm exec` is not necessary to invoke the tool inside
a package since the load path is automatically computed by this tool.
For instance, try (inside this package)

    > cpm-query Data.List split
    > cpm-query System.Process exitWith
    > cpm-query System.Directory doesFileExist
