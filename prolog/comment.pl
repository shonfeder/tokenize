/*
module(tokenize(comment)
       [comment/2,
        comment_rec/2,
        comment_token/2,
        comment_token_rec/2]).
*/

dcgtrue(U,U).

id([X|L]) --> [X],id(L).
id([]) --> dcgtrue.
id([X|L],[X|LL]) --> [X],id(L,LL).
id([],[]) --> dcgtrue.

tr(S,SS) :-
    atom(S) ->
        (
            atom_codes(S,C),
            SS=id(C)
        );
    SS=S.

eol     --> {atom_codes('\n',E)},id(E).
eol(HS) --> {atom_codes('\n',E)},id(E,HS).
 
comment_body(E) --> call(E),!.
comment_body(E) --> [_],comment_body(E).
comment_body(_) --> [].
                           
comment(S,E) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS),
    comment_body(EE).

line_comment(S) -->
    {tr(S,SS)},
    comment_body(SS,eol).

comment_body_token(E,Text) -->
    call(E,HE),!,
    {append(HE,[],Text)}.

comment_body_token(E,[X|L]) -->
    [X],
    comment_body_token(E,L).

comment_body_token(_,[]) --> [].
                           
comment_token(S,E,Text) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS,HS),
    {append(HS,T,Text)},
    comment_body_token(EE,T).

line_comment_token(S,Text) -->
    {tr(S,SS)},
    comment_body_token(SS,eol,Text).

comment_body_rec_cont(S,E,Cont,HE,Text) -->
    {append(HE,T,Text)},
    comment_body_token_rec(S,E,Cont,T).

comment_body_rec_start(HE,Text) -->
    {append(HE,[],Text)}.

comment_body_token_rec(_,E,Cont,Text) -->
    call(E,HE),
    call(Cont,HE,Text).

comment_body_token_rec(S,E,Cont,Text) -->
    call(S,HS),
    {append(HS,T,Text)},
    comment_body_token_rec(S,E,comment_body_rec_cont(S,E,Cont),T).

comment_body_token_rec(S,E,Cont,[X|L]) -->
    [X],
    comment_body_token_rec(S,E,Cont,L).

comment_body_token_rec(_,_,_,_,_,[]) --> dcgtrue.

comment_token_rec(S,E,Text) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS,HS),
    {append(HS,T,Text)},
    comment_body_token_rec(SS,EE,comment_body_rec_start,T).

comment_body_rec(_,E) -->
    call(E).

comment_body_rec(S,E) -->
    call(S),
    comment_body_rec(S,E),
    comment_body_rec(S,E).

comment_body_rec(S,E) -->
    [_],
    comment_body_rec(S,E).

comment_body_rec(_,_).

comment_rec(S,E) -->
    {
        tr(S,SS),
        tr(E,EE)
    },
    call(SS),
    comment_body_rec(SS,EE).

test(Tok,S,U) :-
    atom_codes(S,SS),
    call_dcg(Tok,SS,U).

test_comment(S) :-
    test(comment('<','>'),S,[]).

test_comment_rec(S) :-
    test(comment_rec('<','>'),S,[]).

test_comment_token(S,T) :-
    test(comment_token('<','>',TT),S,[]),
    atom_codes(T,TT).

test_comment_token_rec(S,T) :-
    test(comment_token_rec('<','>',TT),S,[]),
    atom_codes(T,TT).

tester([]).
tester([X|L]) :-
    write_term(test(X),[]),
    (
        call(X) -> write(' ... OK') ; write(' ... FAIL')
    ),
    nl,
    tester(L).


/*
tester(
    [test_comment('<alla>'),
     test_comment_rec('<alla<balla>>'),
     test_comment_token('<alla>','<alla>'),
     test_comment_token_rec('<alla<balla>>','<alla<balla>>')]).
*/

    
        
