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


% NUMBERS

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


% STRINGS

test('Tokenizing the empty strings',
     [true(Actual == Expected)]
    ) :-
    tokenize(`""`, Actual),
    Expected = [string('')].

test('Untokenizing an empty string',
     [true(Actual == Expected)]
    ) :-
    untokenize([string('')], Actual),
    Expected = `""`.

test('Tokenizing a string with just two escapes',
     [true(Actual == Expected)]
    ) :-
    tokenize(`"\\\\"`, Actual),
    Expected = [string('\\\\')].

test('Untokenizing a string with just two characters',
     [true(Actual == Expected)]
    ) :-
    untokenize([string('aa')], Actual),
    Expected = `"aa"`.

test('Extracts a string',
     [true(Actual == Expected)]
    ) :-
    tokenize("\"a string\"", Actual),
    Expected = [string('a string')].

test('Extracts a string among other stuff',
     [true(Actual == Expected)]
    ) :-
    tokenize("Some other \"a string\" stuff", Actual),
    Expected = [word(some),spc(' '),word(other),spc(' '),string('a string'),spc(' '),word(stuff)].

test("Extracts a string that includes escaped brackets",
     [true(Actual == Expected)]
    ) :-
    tokenize(`"a \\"string\\""`, Actual),
    Expected = [string('a "string"')].

test("Tokenization preserves escaped characters",
     [true(Actual == Expected)]
    ) :-
    tokenize(`"\\tLine text\\n"`, Actual),
    Expected = [string('\\tline text\\n')].

test("Extracts a string that includes a doubly nested string",
     [true(Actual == Expected)]
    ) :-
    tokenize(`"a \\"sub \\\\"string\\\\"\\""`, Actual),
    Expected = [string('a "sub \\"string\\""')].

test("Untokenizes string things",
     [true(Actual == Expected)]
    ) :-
    untokenize([string('some string')], ActualCodes),
    string_codes(Actual, ActualCodes),
    Expected = "\"some string\"".

:- end_tests(tokenize).
