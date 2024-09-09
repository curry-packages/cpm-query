cpm-query
=========

This package contains the implementation of a tool to query
analysis information about entities (e.g., function, types)
defined in a module of some Curry package.

The tool is based and, therefore, requires the executable `curry-info`
to manage various Curry analysis tools.

To start the tool:

    > cpm-query <module name> <function name>

If the tool is used inside a package, it should be invoked as

    > cypm exec cpm-query <module name> <function name>

so that functions defined in imported packages can be found.
