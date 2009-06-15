:- use_module(library(chr)).
:- chr_constraint rule/3, sentence/1, sentence/2, word/2, beta/4, ntcount/2.

:- [grammar].

sentence(S) <=> sentence(1,S).
remove_empty_sentence @ sentence(_,[]) <=> true.
split_sentence @
sentence(Index,[Word|R]) <=> 
    NextIndex is Index + 1 | word(Index,Word), sentence(NextIndex,R).

ntcount(N,X), ntcount(N,Y) <=> Z is X+Y | ntcount(N,Z).

% Count rules for each non-terminal
rule(N,_,_) ==> ntcount(N,1).

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
word(Pos,Word), rule(NonTerm,[Word],Prob) \ ntcount(NonTerm,NtCount) <=>
    NtCount > 0,
    NextNtCount is NtCount - 1
    |
    ntcount(NonTerm,NextNtCount),
    beta(NonTerm, Pos, Pos, Prob).

recursion @
rule(NonTermJ, [NonTermR,NonTermS], JProb),
ntcount(NonTermR,0), ntcount(NonTermS,0),
beta(NonTermR, P, D, ProbR),
beta(NonTermS, D1, Q, ProbS) \ ntcount(NonTermJ, NtCountJ) <=>
    NtCountJ > 0,
    NextNtCountJ is NtCountJ - 1,
    D1 is D + 1,
    BetaProb is ProbR * ProbS * JProb,
    write(rule(NonTermJ, [NonTermR,NonTermS], JProb)),
    write(','),
    write(beta(NonTermR, P, D, ProbR)),write(','),
    write(beta(NonTermS, D1, Q, ProbS)), write('==>'),
    write(beta(NonTermJ, P, Q, BetaProb)),nl
    |
    ntcount(NonTermJ, NextNtCountJ),
    beta(NonTermJ, P, Q, BetaProb).

test :-
	sentence([astronomers, saw, stars, with, ears]),
%	sentence([stars,with,ears]),
	init_grammar.
