:- module(tokenize_opts,
          [process_options/3,
           preopts_data/3,
           postopts_data/3]).

:- use_module(library(record)).

% pre-processing options
:- record preopts(
       cased:boolean=false
   ).

% post-processing options
:- record postopts(
       spaces:boolean=true,
       cntrl:boolean=true,
       punct:boolean=true,
       to:oneof([strings,atoms,chars,codes])=atoms,
       pack:boolean=false
   ).

%% process_options(+Options:list(term), -PreOpts:term, -PostOpts:term) is semidet.
%
process_options(Options, PreOpts, PostOpts) :-
    make_preopts(Options, PreOpts, Rest),
    make_postopts(Rest, PostOpts, InvalidOptions),
    throw_on_invalid_options(InvalidOptions).

throw_on_invalid_options(InvalidOptions) :-
    InvalidOptions \= []
    -> throw(invalid_options_given(InvalidOptions))
    ;  true.
