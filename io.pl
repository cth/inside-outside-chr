:- use_module(library(chr)).
:- chr_constraint rule/3, sentence/1, sentence/2, sentence_length/1, word/2, inside/5, level/1, root/1, notroot/1, outside/4, inside_done/0.
%:- set_prolog_flag(chr_toplevel_show_store,false).

:- [grammar].

% Breaking up sentences into constraints
sentence(S) <=> length(S,L) | sentence_length(L), sentence(1,S).
remove_empty_sentence @ sentence(_,[]) <=> true.
split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).

% Find root and nonroot nonterminals
root(N) \ root(N) <=> true.
notroot(N) \ notroot(N) <=> true.
notroot(N) \ root(N) <=> true.
rule(N,_,_) ==> root(N).
%rule(N,[_]) ==> notroot(N).
rule(_,[A,B],_) ==> notroot(A), notroot(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inside algoritm:
inside_sum_multiple @
inside(N,L1,P,Q,Prob1), inside(N,L2,P,Q,Prob2) <=>
	L1 =< L2,
    Prob is Prob1 + Prob2,
    write(inside(N,P,Q,Prob1)),
    write(','),write(inside(N,P,Q,Prob2)),
    write('<=>'),
    write(inside(N,P,Q,Prob)),nl
    |
    inside(N,L2,P,Q,Prob).

inside_base_case @
level(1), % Not strictly necessary
word(Pos,Word), rule(NonTerm,[Word],Prob) ==>
	write('inside (basecase):'),nl, write(word(Pos,Word)), write(','),write(rule(NonTerm,[Word],Prob)), write('==>'), write(inside(NonTerm,1,Pos,Pos,Prob)), nl,
    inside(NonTerm, 1, Pos, Pos, Prob).

inside_recursion @
level(L), rule(NonTermJ, [NonTermR,NonTermS], JProb), inside(NonTermR, L1, P, D, ProbR), inside(NonTermS, L2, D1, Q, ProbS) ==>
    max(L1,L2,L),
    NextLevel is L + 1,
    D1 is D + 1,
    BetaProb is ProbR * ProbS * JProb,
    write('inside: '),
    write(rule(NonTermJ, [NonTermR,NonTermS], JProb)),
    write(','),
    write(inside(NonTermR, L1, P, D, ProbR)),write(','),
    write(inside(NonTermS, L2, D1, Q, ProbS)), write('==>'),
    write(inside(NonTermJ, NextLevel, P, Q, BetaProb)),nl
    |
    inside(NonTermJ, NextLevel, P, Q, BetaProb).

sentence_length(SL) \ level(L) <=>
    L < SL,
    M is L + 1    
    |
    write('increase level: '), write(M), nl,
    level(M).

sentence_length(L), level(L) ==> inside_done.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% outside algorithm

%discard_zero_prob @

outside_base_case1 @
inside_done, sentence_length(L), rule(N,_,_), root(N) ==> outside(N,1,L,1).

%outside_base_case2 @
%inside_done, sentence_length(L), rule(N,_,_), notroot(N) ==> outside(N,1,L,0).


outside_sum_multiple @
inside_done \ 
outside(N,P,Q,P1), outside(N,P,Q,P2) <=>
    Prob is P1 + P2,
    write('outside_sum_multiple: '), write(outside(N,P,Q,P1)), write(','), write(outside(N,P,Q,P2)), write(' <=> '), write(outside(N,P,Q,Prob)),nl,
    outside(N,P,Q,Prob).


outside_left_recursion @
inside_done,
rule(NF,[NJ,NG],PF), outside(NF,P,E,PaF), inside(NG,_,Q1,E,PbG),inside(NJ,_,P,Q,_) ==>
    Q is Q1 - 1,
    PJ is PF * PaF * PbG
    |
    write('outside_left_rec: '), write(rule(NF,[NJ,NG],PF)), write(','), write(outside(NF,P,E,PaF)), write(','), write(inside(NG,_,Q1,E,PbG)), write('==>'), write(outside(NJ,P,Q,PJ)), nl,
    outside(NJ,P,Q,PJ).

outside_right_recursion @
inside_done,
rule(NF,[NG,NJ],PF), outside(NF,P,E,PaF), inside(NG,_,P,Q,PbG), inside(NJ,_,Q1,E,_) ==>
    Q1 is Q + 1,
    PJ is PF * PaF * PbG
    |
    write('outside_right_rec: '), write(rule(NF,[NG,NJ],PF)), write(','), write(outside(NF,P,E,PaF)), write(','), write(inside(NG,_,P,Q,PbG)), write(','), write(inside(NJ,_,Q1,E,_)), write('==>'), write(outside(NJ,Q1,E,PJ)), nl,
    outside(NJ,Q1,E,PJ).

%% Prolog helpers:

max(A,B,A) :- B =< A.
max(A,B,B) :- A < B.

test :-
	init_grammar,
%	sentence([astronomers, saw, stars, with, ears]),
	sentence([astronomers,saw,stars]),
%	sentence([stars]),
	level(1).
