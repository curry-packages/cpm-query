cpm-query
=========

This package contains the implementation of a tool to query
analysis information about entities (e.g., function, types, type classes)
defined in a module of some Curry package.

The tool is based and, therefore, requires the executable `curry-info`
to manage various Curry analysis tools.
`curry-info` can be installed as follows:

    > git clone https://github.com/curry-language/curry-info-system.git
    > cd curry-info-system
    > cypm install

To start the `cpm-query` tool:

    > cpm-query <module name> <function name>

or

    > cpm-query --type <module name> <type name>

or

    > cpm-query --class <module name> <class name>

Note that it is not necessary to invoke the tool inside a package
with `cypm exec` since the load path is automatically computed by this tool.
For instance, try (inside this package)

    > cpm-query Data.List split
    > cpm-query --class Prelude Ord
    > cpm-query System.Process exitWith
    > cpm-query System.Directory doesFileExist
