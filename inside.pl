:- use_module(library(chr)).

:- chr_constraint rule/3, sentence/1, sentence/2, word/2, beta/4.

sentence(S) <=> sentence(1,S).
remove_empty_sentence @ sentence(_,[]) <=> true.
split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).

sum_multiple @
beta(N,P,Q,Prob1), beta(N,P,Q,Prob2) <=>
    Prob is Prob1 + Prob2
    |
    beta(N,P,Q,Prob).

base_case @
word(Pos,Word), rule(NonTerm,[Word],Prob) ==> beta(NonTerm, Pos, Pos, Prob).

recursion @
rule(NonTermJ, [NonTermR,NonTermS], JProb),
beta(NonTermR, P, D, ProbR), beta(NonTermS, D1, Q, ProbS) ==>
   BetaProb is ProbR * ProbS * JProb
   |
   beta(NonTermJ, P, Q, BetaProb).

init_grammar :- 
    rule(np, [det,noun],1.0),
    rule(det, [the], 1.0),
    rule(noun, [boy], 1.0).

