:- chr_constraint rule/3, sentence/1, word/2.

remove_empty_sentence @ sentence(_,[]) <=> true.
split_sentence @
sentence(Time,[Elem|R]) <=> 
    NextTime is Time + 1 | input(Time,Elem), sentence(NextTime,R).

%rule(np, [np,vp], 0.2)

% beta(J, From, To)


