#!/usr/bin/env swipl
/** <module>  Developer convenience for the tokenize library
 *
 * To run the unit tests, execute this file
 *
 *    ./test/test.pl
 *
 * To run this file just consult and call test. Mostly useful to get
 * the file consulted
 */


:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% Add the package source files relative to the current file location
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../prolog', PackageDir),
   asserta(user:file_search_path(tokenize, PackageDir)).

:- use_module(tokenize(tokenize)).

% TESTS START HERE

test('Hello, Tokenize!',
     [true(Actual == Expected)]
    ) :-
    tokenize("Hello, Tokenize!", Actual),
    Expected = [word(hello),punct(','),spc(' '),word(tokenize),punct(!)].

test('Goodbye, Tokenize!',
     [true(Actual == Expected)]
    ) :-
    Tokens = [word('Goodbye'),punct(','),spc(' '),word('Tokenize'),punct('!')],
    untokenize(Tokens, Codes),
    string_codes(Actual, Codes),
    Expected = "Goodbye, Tokenize!".

test('7.0',
     [true(Actual == Expected)]
    ) :-
    tokenize("7.0", Actual),
    Expected = [number(7.0)].


