:- use_module(library(chr)).

:- chr_constraint
	sentence/1, sentence/2, word/2, sentence_length/1,
        rule/3, root/1, notroot/1,
	inside/5, level/1, inside_done/0,
	outside/4.

max(A,B,A) :- A > B.
max(A,B,B) :- A =< B.


%:- set_prolog_flag(chr_toplevel_show_store,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities for preprocessing sentence constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sentence_properties @
sentence(S) <=> length(S,L) | sentence_length(L), sentence(1,S).

remove_empty_sentence @ sentence(_,[]) <=> true.

split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities for preprocessing grammar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find root and nonroot nonterminals

remove_dup_root @
root(N) \ root(N) <=> true.

remove_dup_notroot @
notroot(N) \ notroot(N) <=> true.

remove_false_roots @
notroot(N) \ root(N) <=> true.

add_root @
rule(N,_,_) ==> root(N).

add_notroot @
rule(_,[A,B],_) ==> notroot(A), notroot(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The inside algorithm implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_sum_multiple @
inside(N,L1,P,Q,Prob1), inside(N,L2,P,Q,Prob2) <=>
    L1 =< L2,
    Prob is Prob1 + Prob2
    |
    inside(N,L2,P,Q,Prob).

inside_base_case @
level(1), % Not strictly necessary
word(Pos,Word), rule(NonTerm,[Word],Prob) ==>
    inside(NonTerm, 1, Pos, Pos, Prob).

inside_recursion @
level(L), rule(NonTermJ, [NonTermR,NonTermS], JProb),
inside(NonTermR, L1, P, D, ProbR), inside(NonTermS, L2, D1, Q, ProbS) ==>
    max(L1,L2,L),
    NextLevel is L + 1,
    D1 is D + 1,
    BetaProb is ProbR * ProbS * JProb
    |
    inside(NonTermJ, NextLevel, P, Q, BetaProb).

increase_tree_level @
sentence_length(SL) \ level(L) <=>
    L < SL,
    M is L + 1
    |
    level(M).

infer_inside_done @
sentence_length(L), level(L) ==> inside_done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The outside algorithm implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

outside_base_case1 @
inside_done, sentence_length(L), rule(N,_,_), root(N) ==> outside(N,1,L,1).

outside_sum_multiple @
inside_done \ 
outside(N,P,Q,P1), outside(N,P,Q,P2) <=>
    Prob is P1 + P2,
    outside(N,P,Q,Prob).

outside_left_recursion @
inside_done,
rule(NF,[NJ,NG],PF), outside(NF,P,E,PaF), inside(NG,_,Q1,E,PbG),inside(NJ,_,P,Q,_) ==>
    Q is Q1 - 1,
    PJ is PF * PaF * PbG
    |
    outside(NJ,P,Q,PJ).

outside_right_recursion @
inside_done,
rule(NF,[NG,NJ],PF), outside(NF,P,E,PaF), inside(NG,_,P,Q,PbG), inside(NJ,_,Q1,E,_) ==>
    Q1 is Q + 1,
    PJ is PF * PaF * PbG
    |
    outside(NJ,Q1,E,PJ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testing stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [grammar].% A small sample grammar

test :-
	init_grammar,
%	sentence([astronomers, saw, stars, with, ears]),
	sentence([astronomers,saw,stars]),
	level(1).
