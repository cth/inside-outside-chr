init_grammar :- 
    rule(np, [det,noun],1.0),
    rule(det, [the], 1.0),
    rule(noun, [boy], 1.0).