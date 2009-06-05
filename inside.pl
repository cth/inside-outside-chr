:- use_module(library(chr)).
:- chr_constraint rule/3, sentence/1, sentence/2, word/2, beta/4.

:- [grammar].

sentence(S) <=> sentence(1,S).
remove_empty_sentence @ sentence(_,[]) <=> true.
split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).

sum_multiple @
beta(N,P,Q,Prob1), beta(N,P,Q,Prob2) <=>
    Prob is Prob1 + Prob2,
    write(beta(N,P,Q,Prob1)),
    write(','),write(beta(N,P,Q,Prob2)),
    write('<=>'),
    write(beta(N,P,Q,Prob)),nl
    |
    beta(N,P,Q,Prob).

base_case @
word(Pos,Word), rule(NonTerm,[Word],Prob) ==> beta(NonTerm, Pos, Pos, Prob).

recursion @
rule(NonTermJ, [NonTermR,NonTermS], JProb),
beta(NonTermR, P, D, ProbR), beta(NonTermS, D1, Q, ProbS) ==>
   D1 is D + 1,
   BetaProb is ProbR * ProbS * JProb,
   write(rule(NonTermJ, [NonTermR,NonTermS], JProb)),
   write(','),
   write(beta(NonTermR, P, D, ProbR)),write(','),
   write(beta(NonTermS, D1, Q, ProbS)), write('==>'),
   write(beta(NonTermJ, P, Q, BetaProb)),nl
   |
   beta(NonTermJ, P, Q, BetaProb).

test :-
	sentence([astronomers, saw, stars, with, ears]),
%	sentence([stars,with,ears]),
	init_grammar.
