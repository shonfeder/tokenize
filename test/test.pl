:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% Add the package source files relative to the current file location
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../prolog', PackageDir),
   asserta(user:file_search_path(package, PackageDir)).

:- use_module(package(tokenize)).
:- begin_tests(tokenize).

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


test('tokenize 7.0',
     [true(Actual == Expected)]
    ) :-
    tokenize("7.0", Actual),
    Expected = [number(7.0)].

test('untokenize 6.3',
     [true(Actual == Expected)]
    ) :-
    untokenize([number(6.3)], Actual),
    Expected = `6.3`.


test('tokenize number in other stuff',
     [true(Actual == Expected)]
    ) :-
    tokenize("hi 7.0 x", Actual),
    Expected = [word(hi), spc(' '), number(7.0), spc(' '), word(x)].

test('untokenize 6.3 in other stuff',
     [true(Actual == Expected)]
    ) :-
    untokenize([word(hi), number(6.3)], Actual),
    Expected = `hi6.3`.


:- end_tests(tokenize).
