:- use_module(library(chr)).
:- chr_constraint rule/3, sentence/1, sentence/2, sentence_length/1, word/2, inside/5, level/1, root/1, notroot/1, outside/4.
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
	write('base-case'),nl,
    inside(NonTerm, 1, Pos, Pos, Prob).

inside_recursion @
level(L), rule(NonTermJ, [NonTermR,NonTermS], JProb), inside(NonTermR, L1, P, D, ProbR), inside(NonTermS, L2, D1, Q, ProbS) ==>
	max(L1,L2,L),
	NextLevel is L + 1,
    D1 is D + 1,
    BetaProb is ProbR * ProbS * JProb,
    write(rule(NonTermJ, [NonTermR,NonTermS], JProb)),
    write(','),
    write(inside(NonTermR, L1, P, D, ProbR)),write(','),
    write(inside(NonTermS, L2, D1, Q, ProbS)), write('==>'),
    write(inside(NonTermJ, NextLevel, P, Q, BetaProb)),nl
    |
    inside(NonTermJ, NextLevel, P, Q, BetaProb).

sentence_length(SL) \ level(L) <=> L < SL + 1 | write('increase level'), nl, M is L + 1, level(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% outside algorithm
outside_base_case1 @
sentence_length(L), rule(N,_,_), root(N) ==> outside(N,1,L,1).

outside_base_case2 @
sentence_length(L), rule(N,_,_), notroot(N) ==> outside(N,1,L,0).


outside_sum_multiple @
outside(N,P,Q,P1), outside(N,P,Q,P2) <=> Prob is P1 * P2, outside(N,P,Q,Prob).
outside_left_recursion @
rule(NF, [NJ, NG],  PF), outside(NF,P,E,PaF), inside(NG, _Lvl, Q1, E, PbG) ==>
	Q is Q1 - 1,
	PJ is PF * PaF * PbG
	|
	outside(NJ,P,Q,PJ).
	
outside_right_recursion @
rule(NF, [NJ, NG],  PF), outside(NF,P,E,PaF), inside(NG, _Lvl, P, Q, PbG) ==>
	Q1 is Q + 1,
	PJ is PF * PaF * PbG
	|
	outside(NJ,Q1,E,PJ).
	
%% Prolog helpers:

max(A,B,A) :- B =< A.
max(A,B,B) :- A < B.

test :-
	sentence([astronomers, saw, stars, with, ears]),
	init_grammar,
	level(1).
