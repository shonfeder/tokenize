:- module(tokenize,
          [ tokenize/2,
            tokenize/3,
            tokenize_file/2,
            tokenize_file/3,
            untokenize/2
          ]).

/** <module> tokenize

This module offers a simple tokenizer with some basic options.

It may be improved towards a cleaner and more extensible tool if there
is enough interest (from myself or others).

@author Shon Feder
@license <http://unlicense.org/>

Rational:

tokenize_atom/2, in library(porter_stem), is inflexible, in that it doesn't
allow for the preservation of white space or control characters, and
it only tokenizes into a list of atoms. This module allows for options to
include or exclude things like spaces and punctuation, and for packing tokens.

It also provides a simple predicate for reading lists of tokens back into
text.

Ideally, provided I have the drive and/or there is any interest in this package,
this would become an extensible, easily configurable tokenizing utility.

*/

%% untokenize(+Tokens:list(term), -Untokens:list(codes)) is semidet.
%
%   True when Untokens is unified with a code list representation of each
%   token in Tokens.

% TODO  structure(Options:[lines, brackets])
% TODO  mode(generate) ; mode(parse)
% TODO  add output format option

untokenize(Tokens, Untokens) :-
    untokenize(Tokens, Untokens, []).
untokenize(Tokens, Untokens, _Options) :-
    maplist(token_to(codes), Tokens, TokenCodes),
    phrase(non_tokens(TokenCodes), Untokens).

non_tokens([T])    --> T.
non_tokens([T|Ts]) --> T, non_tokens(Ts).

%% tokenize_file(+File:atom, -Tokens:list(term)) is semidet.
%
%   @see tokenize_file/3 is called with an empty list of options: thus, with defaults.
%

% Note: does not use phrase_from_file/3, thus not lazy or transparent
% This choice was made so that tokenize_file will work with remotely
% accessed files.

% TODO: add more source options

tokenize_file(File, Tokens) :-
    tokenize_file(File, Tokens, []).

%% tokenize_file(+File:atom, -Tokens:list(term), +Options:list(term)) is semidet.
%
%   True when Tokens is unified with a list of tokens represening
%   the text of File.
%
%   @see tokenize/3 which has the same available options and behavior.

tokenize_file(File, Tokens, Options) :-
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    tokenize(Codes, Tokens, Options).

%% tokenize(+Text:list(code), -Tokens:list(term)) is semidet.
%
%   @see tokenize/3 is called with an empty list of options: thus, with defaults.

% TODO: add support for unicode

tokenize(Text, Tokens) :-
    tokenize(Text, Tokens, []).

%% tokenize(+Text:list(code), -Tokens:list(term), +Options:list(term)) is semidet.
%
%   True when Tokens is unified with a list of tokens representing the text from
%   Text, according to the options specified in Options.
%
%   NOTE: this predicate currently fails if invalid option arguments are given
%   and, worse, it succeeds silently if there are invalid option parameters.
%
%   A token is one of:
%
%   * a word (contiguous alpha-numeric chars): `word(W)`
%   * a punctuation mark (determined by `char_type(C, punct)`): `punct(P)`
%   * a control character (determined by `char_typ(C, cntrl)`): `cntrl(C)`
%   * a space ( == ` `): `spc(S)`.
%
%  Valid options are:
%
%   * cased(+bool)  : Determines whether tokens perserve cases of the source text.
%   * spaces(+bool) : Determines whether spaces are represted as tokens or discarded.
%   * cntrl(+bool)  : Determines whether control characters are represented as tokens or discarded.
%   * punct(+bool)  : Determines whether punctuation characters are represented as tokens or discarded.
%   * to(+on_of([strings,atoms,chars,codes])) : Determines the representation format used for the tokens.
%   * pack(+bool)   : Determines whether tokens are packed or repeated.

tokenize(Text, Tokens, Options) :-
    string_codes(Text, Codes),
    phrase(process_options, [Options-Codes], [Options-Tokens]).

% PROCESSING OPTIONS
%
%   NOTE: This way of processing options is probably stupid.
%   I will correct/improve/rewrite it if there is ever a good
%   reason to. But for now, it works.
%
%   TODO: Throw exception if invalid options are passed in.
%   At the moment it just fails.

%% Dispatches dcgs by option-list functors, with default values.
process_options -->
    opt(cased,  false),
    non_opt(tokenize_text),
    opt(spaces, true),
    opt(cntrl,  true),
    opt(punct,  true),
    opt(to,     atoms),
    opt(pack,   false).

%% opt(+OptionFunctor:atom, DefaultValue:nonvar)
%
%   If dcg functor is identical to the option name with 'opt_' prefixed,
%   then the dcg functor can be omitted.

opt(Opt, Default) -->
    { atom_concat('opt_', Opt, Opt_DCG) },
    opt(Opt, Default, Opt_DCG).

%% opt(+OptionFunctor:atom, +DefaultValue:nonvar, +DCGFunctor:atom).
opt(Opt, Default, DCG) -->
    state(Opts-Text0, Text0),
    {
        pad(Opt, Selection, Opt_Selection),
        option(Opt_Selection, Opts, Default),
        DCG_Selection =.. [DCG, Selection]
    },
    DCG_Selection,
    state(Text1, Opts-Text1).
%% This ugly bit should be dispensed with...
opt(Opt, Default, _) -->
    state(Opts-_),
    {
        var(Default), \+ option(Opt, Opts),
        writeln("Unknown options passed to opt//3: "),
        write(Opt)
    }.

%% non_opt(+DCG).
%
%   Non optional dcg to dispatch. Passes the object of concern
%   without the options list, then recovers option list.

non_opt(DCG) -->
    state(Opts-Text0, Text0),
    DCG,
    state(Text1, Opts-Text1).

state(S0),     [S0] --> [S0].
state(S0, S1), [S1] --> [S0].

%% Dispatching options:

opt_cased(true)  --> [].
opt_cased(false) --> state(Text, LowerCodes),
    {
        text_to_string(Text, Str),
        string_lower(Str, LowerStr),
        string_codes(LowerStr, LowerCodes)
    }.

tokenize_text --> state(Text, Tokenized),
    { phrase(tokens(Tokenized), Text) }.

opt_spaces(true)  --> [].
opt_spaces(false) --> state(T0, T1),
    { exclude( =(spc(_)), T0, T1) }.

opt_cntrl(true)  --> [].
opt_cntrl(false) --> state(T0, T1),
    { exclude( =(cntrl(_)), T0, T1) }.

opt_punct(true)  --> [].
opt_punct(false) --> state(T0, T1),
    { exclude( =(punct(_)), T0, T1) }.

opt_to(codes) --> [].
opt_to(Type)  --> state(CodeTokens, Tokens),
    { maplist(token_to(Type), CodeTokens, Tokens) }.

opt_pack(false) --> [].
opt_pack(true)  --> state(T0, T1),
    { phrase(pack_tokens(T1), T0) }.



%% POST PROCESSING

%% Convert tokens to alternative representations.
token_to(Type, Token, Converted) :-
    ( Type == strings -> Conversion = inverse(string_codes)
    ; Type == atoms   -> Conversion = inverse(atom_codes)
    ; Type == chars   -> Conversion = inverse(string_chars)
    ; Type == codes   -> Conversion = string_codes
    ),
    call_into_term(Conversion, Token, Converted).


%% Packing repeating tokens
%
pack_tokens([T])    --> pack_token(T).
pack_tokens([T|Ts]) --> pack_token(T), pack_tokens(Ts).

pack_token(P) --> pack(Token, N), {Token =.. [F,T], P =.. [F,T,N]}.

pack(X, Count) --> [X], pack(X, 1, Count).
pack(_, Total, Total)      --> call(eos).
pack(X, Total, Total), [Y] --> [Y], { Y \= X }.
pack(X, Count, Total)      --> [X], { succ(Count, NewCount) },
                               pack(X, NewCount, Total).



%% PARSING

tokens([T])    --> token(T), call(eos), !.
tokens([T|Ts]) --> token(T), tokens(Ts).
%% tokens(_)   --> {length(L, 200)}, L, {format(L)}, halt, !. % For debugging.

token(word(W))     --> word(W), call(eos), !.
token(word(W)),` ` --> word(W), ` `.
token(word(W)), C  --> word(W), (punct(C) ; cntrl(C) ; nasciis(C)).
token(spc(S))      --> spc(S).
token(punct(P))    --> punct(P).
token(cntrl(C))    --> cntrl(C).
token(other(O))    --> nasciis(O).


spc(` `) --> ` `.

sep --> ' '.
sep --> call(eos), !.

word(W) --> csyms(W).

csyms([L])    --> csym(L).
csyms([L|Ls]) --> csym(L), csyms(Ls).

csym(L)       --> [L], {code_type(L, csym)}.


% non ascii's
nasciis([C])     --> nascii(C), (call(eos), !).
nasciis([C]),[D] --> nascii(C), [D], {D < 127}.
nasciis([C|Cs])  --> nascii(C), nasciis(Cs).

nascii(C)        --> [C], {C > 127}.

%% blanks --> [].
%% blanks --> ' '.

' ' --> space.
' ' --> space, ' '.

... --> [].
... --> [_], ... .

space --> [S], {code_type(S, white)}.

punct([P]) --> [P], {code_type(P, punct)}.
cntrl([C]) --> [C], {code_type(C, cntrl)}.

eos([], []).

%% move to general module

codes_to_lower([], []).
codes_to_lower([U|Uppers], [L|Lowers]) :-
    code_type(U, to_upper(L)),
    codes_to_lower(Uppers, Lowers).

call_into_term(P, Term, Result) :-
    Term =.. [F, Arg],
    call(P, Arg, ResultArg),
    Result =.. [F, ResultArg].

inverse(P, A, B) :-
    call(P, B, A).

pad(T_Args, X, T_X_Args) :-
    T_Args   =.. [T|Args],
    T_X_Args =.. [T, X| Args].
