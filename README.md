# Enumerator for (pseudo) regular languages

[![Build Status](https://travis-ci.org/tadeboro/reglang.svg?branch=master)]
(https://travis-ci.org/tadeboro/reglang)

This project's aim is to build regular language enumerator that can be
used when generating strings with specific layouts (often needed when
testing text processing applications or creating text corpuses).


## Inspiration

Main idea of the project is based on the article [Enumerating the
strings of regular languages](http://www.cs.dartmouth.edu/~doug/nfa.ps.gz).
This project expanded prior work by adding ability to capture group
contents and use it when generating strings and attaching a parser for
regular expressions to make it easier to enter them.


## Installation

After cloning repository, execute following sequence of commands:

    $ cabal configure
    $ cabal build
    $ cabal install

This will install library files into proper locations and also install
executable `listlang` into bin folder.

If one wants to test sources using built-in test suite, proper sequence
of commands to execute is:

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

Generating API docs is also similarly simple:

    $ cabal haddock


## Usage

Functionality of this package can be consumed in two ways: by using
provided executable `listlang` or by directly using provided packages in
application development.


### Using standalone program

Using standalone program is quite straightforward. Simply run

    $ listlang REGEX

to enumerate regular expresion REGEX, passed in as string. For example,
to generate all character pairs, we can execute

    $ listlang ".."

and get back list of strings, one per line. This can be quite usefull in
constructs such as *for loops* in bash, where we can capture output and
iterate over it, probably feeding it into some application for further
processing.

It is possible to modify output of this program by providing additional
switches when executing command. More information about possible
switches can be found by executing `listlang` with no parameters.


### Using library in application

When used as a library, this package is most useful when used in
combination with `QuickCheck`, where it can serve as a data source. For
complete demonstration of how to utilise this library as a data source,
have a look at `examples/qc.hs` script.
