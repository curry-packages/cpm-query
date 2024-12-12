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
