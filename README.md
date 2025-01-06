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


Generating analysis information
-------------------------------

`cpm-query` can also be used to generate analysis information
with `curry-info`. For instance, the generation of analysis information
for all modules in package `base` in version `3.3.0` can be done by

    > cpm-query --generate base 3.3.0

This generates some default information for each class, type, and operation
in all modules. To generate specific information (i.e., requests
supported by `curry-info`), one can use the `--request` option.
For instance, the signatures of all operations of package `process`
with version `3.0.0` can be generated by

    > cpm-query --generate --operation --request=signature process 3.0.0

One can also put a list of package/version information
in a file (i.e., each line contains a package name and a version)
and provides this file as an option:

    > cpm-query --generate --from=FILE

If the file name is `-`, the package/version pairs will be read from
standard input.
