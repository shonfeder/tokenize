:- module(tokenize,
          [ tokenize/2,
            tokenize/3,
            tokenize_file/2,
            tokenize_file/3,
            untokenize/2
          ]).

%% Rational:
%
%   tokenize_atom/2, in library(porter_stem), is inflexible, in that it doesn't
%   allow for the preservation of white space or control characters, and
%   it only tokenizes into a list of atoms...
%
%   Ideally, provided I have the drive and/or there is any interest in this package,
%   this would become an extensible, easily configurable tokenizing utilty.
%
% TODO:
%

%% untokenize(+Tokens, ?Untokens).
%
%   Should take a list of tokenized terms and generate text in desired format.
%
%   TODO:
%
%   - add options:
%       - structure(Options:[lines, brackets])
%       - mode(generate) ; mode(parse)
%       - add output format option

untokenize(Tokens, Untokens) :-
    untokenize(Tokens, Untokens, []).
untokenize(Tokens, Untokens, _Options) :-
    maplist(token_to(codes), Tokens, TokenCodes),
    phrase(non_tokens(TokenCodes), Untokens).

non_tokens([T])    --> T.
non_tokens([T|Ts]) --> T, non_tokens(Ts).

%% Tokenization from various sources.
%
%   Note: does not use phrase_from_file/3, thus not lazy or transparent
%   This choice was made so that tokenize_file will work with remotely
%   accessed files.

%% TODO:
%
%   add more source options

tokenize_file(File, Tokens) :-
    tokenize_file(File, Tokens, []).

tokenize_file(File, Tokens, Options) :-
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    tokenize(Codes, Tokens, Options).

%% Tokenize text:
%
%   A token is one of:
%       - a word (contiguous alpha-numeric chars): `word(W)`
%       - a punctuation mark (determined by `char_type(C, punct)`): `punct(P)`
%       - a control character (determined by `char_typ(C, cntrl)`): `cntrl(C)`
%       - a space ( == ` `): `spc(S)`.
%
%   Supports options:
%       TODO:
%          - add support for unicode

tokenize(Text, Tokens) :-
    tokenize(Text, Tokens, []).

tokenize(Text, Tokens, Options) :-
    string_codes(Text, Codes),
    phrase(process_options, [Options-Codes], [Options-Tokens]).

%% PROCESSING OPTIONS
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
    opt(spaces, false),
    opt(cntrl,  true),
    opt(punct,  true),
    opt(to,     atom),
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
    ( Type == string -> Conversion = inverse(string_codes)
    ; Type == atom   -> Conversion = inverse(atom_codes)
    ; Type == chars  -> Conversion = inverse(string_chars)
    ; Type == codes  -> Conversion = string_codes
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
