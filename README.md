cpm-query
=========

This package contains the implementation of a tool to query
analysis and verification information about entities
(e.g., operations, types, type classes)
defined in a module of some Curry package.

This tool is based on
[CurryInfo](https://github.com/curry-language/curry-info-system.git),
a system to collect and provide analysis and verification information
about Curry modules contained in Curry packages maintained by the
[Curry Package Manager](https://curry-lang.org/tools/cpm/).

In the default mode, a
[web service of CurryInfo](https://cpm.curry-lang.org/webapps/curry-info/run.cgi?-V)
is used to request information about Curry entities.
If the web service is not available, one can also use a local installation
of CurryInfo. In this case, the executable `curry-info` must be installed
as follows:

    > git clone https://github.com/curry-language/curry-info-system.git
    > cd curry-info-system
    > cypm install

However, the data repository of CurryInfo needs to be populated
for which other analysis and verification tools are required,
which is described in the repository of CurryInfo.
Thus, it is recommended for easy use to use `cpm-query` in the default
web service mode.

To get some information about operations, types, or classes defined
in a Curry module, one can start the `cpm-query` tool as follows:

    > cpm-query <module name> <operation name>

or

    > cpm-query --type <module name> <type name>

or

    > cpm-query --class <module name> <class name>

In this simple mode, the specified module must be accessible
in the current load path of Curry
(note that it is not necessary to invoke the tool inside a package
with `cypm exec` since the load path is automatically computed by this tool).
For instance, try (inside this package)

    > cpm-query Prelude foldr1
    > cpm-query Data.List split
    > cpm-query --class Prelude Ord

or, inside this package,

    > cpm-query System.Process exitWith
    > cpm-query System.Directory doesFileExist

To get some information about a module defined in some other package,
one has to provide options for the package name and version, like

    > cpm-query -p xml -x 3.0.0 XML --type XmlExp

One can also omit the entity (operation, type, class) so that one obtains
some information about the module itself:

    > cpm-query -p xml -x 3.0.0 XML

If one is interested in other pieces of information, one can use the
`--request` option to specify requests supported by CurryInfo.
For instance, the following command shows the documentation comments and
signature of all operations defined in module `Data.Maybe`:

    > cpm-query --all --request=documentation,signature Data.Maybe

One can also omit the module name to request information about a specific
version of a package. For instance, the list of modules of package
`base` with version `3.3.0` can be queried by

    > cpm-query -p base -x 3.3.0 --request=modules


Configuration
-------------

As shown above, one can influence the behavior of `cpm-query` by setting
various options. Another configuration method is to place definitions
in the file `$HOME/.cpmqueryrc`. This file is generated when `cpm-query`
is called for the first time. If one is not happy with the default
requests shown by `cpm-query`, one can define in this file other
requests for classes, types, and operations.
This is useful when `cpm-query` is automatically invoked,
as in the REPL command `:info` of
[PAKCS](https://www.curry-lang.org/pakcs/) (version 3.8.0 or higher)
or in the
[Curry Language Server](https://github.com/fwcd/curry-language-server)
(see below).


Usage in the Curry Language Server
----------------------------------

The [Curry Language Server](https://github.com/fwcd/curry-language-server)
supports an extension for invoking programs when hovering over
particular program entities, like operations or types.
If one uses `cpm-query`, one can see analysis and verification
information when hovering over an entity defined in some module
of an existing Curry package.
The image below shows a screenshot when hovering over `length`.

![Analysis infos for length](https://cpm.curry-lang.org/PACKAGES/cpm-query-0.1.0/images/cpm-query-length.png)

In order to activate this possibility, one has to configure
the Curry Language Server inside Visual Studio Code as follows.

1. Select `File > Preferences > Settings`
2. Go to `Extensions > Curry > Language Server: Extensions` and
   select `Edit in settings.json`
3. Add the entry (or adapt it according to your preferences)

        "curry.languageServer.extensions": [
            {
                "name": "Analysis Infos",
                "extensionPoint": "hover",
                "executable": "cpm-query",
                "args": [
                    "--maxtime=3",
                    "--clskind={symbolKind}",
                    "{module}",
                    "{identifier}"
                ]
            }
        ]

Note that the executable `cpm-query` must be accessible in your path.
When you now hover over some operation imported from some
existing Curry package maintained by
[CPM](https://www.curry-lang.org/tools/cpm/),
the executable `cpm-query` is invoked to show the analysis
information about this operation in a pop-up window.


Generating analysis information
-------------------------------

`cpm-query` can also be used to generate analysis information
with `curry-info` in order to fill the data repository of CurryInfo.
This is only relevant if you want to maintain a local version
of CurryInfo.

For instance, analysis information for all modules in package `base`
in version `3.3.0` can be generated by

    > cpm-query --generate base 3.3.0

This cleans all old information for this package and generates
the information for each class, type, and operation
for all modules of this package. To generate specific information
(i.e., requests supported by `curry-info`), one can use the `--request` option.
For instance, the documentation of all operations of package `process`
with version `3.0.0` can be generated by

    > cpm-query --generate --operation --request=documentation process 3.0.0

One can also add a module name to generate information for this module only.
For instance, the documentation of all prelude types can be generated by

    > cpm-query --generate base 3.3.0 Prelude --type --request=documentation

One can also put a list of package/version information
in a file (i.e., each line contains a package name and a version)
and provide the file name as an option:

    > cpm-query --generate --from=FILE

If the file name is `-`, the package/version pairs will be read from
standard input.

Note that this information is locally generated unless the option
`--remote` is explicitly given.
