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

:- end_tests(tokenize).
