#!/usr/bin/env swipl
/** <module>  Unit tests for the tokenize library
 *
 * To run these tests, execute this file
 *
 *    ./test/test.pl
 */

:- initialization(main, main).

main(_Argv) :-
    run_tests.

:- begin_tests(tokenize).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% Add the package source files relative to the current file location
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../prolog', PackageDir),
   asserta(user:file_search_path(package, PackageDir)).

:- use_module(package(tokenize)).

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

:- end_tests(tokenize).
