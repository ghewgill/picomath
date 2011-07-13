picomath
========

Picomath is a collection of small math functions inspired by John D. Cook's [Stand-alone code for numerical computing](http://www.johndcook.com/stand_alone_code.html). 
It is not compiled into a *library* in the usual sense (that is, something you can link to).
Rather, the functions are presented as standalone snippets of code.

License
-------

The picomath library is in the **public domain**.
Do whatever you want with it, no strings attached.
Use at your own risk.

Features
--------

Picomath provides the following functions:

- [Error function](http://en.wikipedia.org/wiki/Error_function)
- [Phi (standard normal CDF)](http://en.wikipedia.org/wiki/Cumulative_distribution_function)
- Phi inverse
- [Gamma](http://en.wikipedia.org/wiki/Gamma_function)
- Log Gamma
- exp(x) - 1 (for small x)
- log(n!)

Languages
---------

The functions are implemented in the following languages:

- C++
- C#
- Erlang
- Java
- Javascript
- Lua
- PHP
- Perl
- Python (2.x and 3.x)
- Ruby
- Tcl

Usage
-----

The picomath library is organised into subdirectories, with each language in a subdirectory.
Each function (or group of interrelated functions such as Gamma and Log Gamma) are implemented in one source file.
Each source file is designed with minimal dependencies, so it can be included directly into a project as is, or copy and pasted into other code.

Test Suite
----------

The test suite consists of two parts:

- an `SConstruct` file (for [SCons](http://scons.org), a command line build system) to build the test code as needed
- a Python `test.py` driver at the top level, and a corresponding `test.*` for each language

To build the tests:

    $ scons

To run the tests:

    $ python test.py

The expected output is:

    Checking cpp... ok
    Checking csharp... ok
    Checking erlang... ok
    Checking java... ok
    Checking javascript... ok
    Checking lua... ok
    Checking perl... ok
    Checking php... ok
    Checking python... ok
    Checking ruby... ok
    Checking tcl... ok

The top level `test.py` runs each language-specific `test.*` in turn, communicating with the implementation using standard input and output.
A simple text-based protocol is used to execute functions and return results.

About
-----

- [John D. Cook](http://www.johndcook.com) is the original author of most of the functions themselves.
- [Greg Hewgill](http://hewgill.com) implemented the test framework and translated the functions to many of the included languages.
