init_grammar :-
    rule(s,[np,vp],1.0),
    rule(pp,[p,np],1.0),
    rule(vp,[v,np],0.7),
    rule(vp,[vp,pp],0.3),
    rule(p,[with],1.0),
    rule(v,[saw],1.0),
    rule(np,[np,pp],0.4),
    rule(np,[astronomers],0.1),
    rule(np,[ears],0.18),
    rule(np,[saw],0.04),
    rule(np,[stars],0.18),
    rule(np,[telescopes],0.1).