% Prolog representation of a grammar to ask a query of a database
% Builds a query which can then be asked of the knowledge base
%  This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is expanded code of Figure 13.12 in Section 13.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2017

% Copyright (c) David Poole and Alan Mackworth 2017. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(L0,L4,Entity,C0,C4) is true if
%  L0 and L4 are list of words, such that
%        L4 is an ending of L0
%        the words in L0 before L4 (written L0-L4) form a noun phrase
%  Entity is an individual that the noun phrase is referring to
% C0 is a list such that C4 is an ending of C0 and C0-C4 contains the constraints imposed by the noun phrase

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0,L4,Entity,C0,C4) :-
    det(L0,L1,Entity,C0,C1),
    adjectives(L1,L2,Entity,C1,C2),
    noun(L2,L3,Entity,C2,C3),
    mp(L3,L4,Entity,C3,C4).
noun_phrase(L0,L4,Entity,C0,C4) :-
    proper_noun(L0,L4,Entity,C0,C4).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | L],L,_,C,C).
det([a | L],L,_,C,C).
det(L,L,_,C,C).


% adjectives(L0,L2,Entity,C0,C2) is true if
% L0-L2 is a sequence of adjectives imposes constraints C0-C2 on Entity
adjectives(L0,L2,Entity,C0,C2) :-
    adj(L0,L1,Entity,C0,C1),
    adjectives(L1,L2,Entity,C1,C2).
adjectives(L,L,_,C,C).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing
mp(L0,L2,Subject,C0,C2) :-
    reln(L0,L1,Subject,Object,C0,C1),
    noun_phrase(L1,L2,Object,C1,C2).
mp([that|L0],L2,Subject,C0,C2) :-
    reln(L0,L1,Subject,Object,C0,C1),
    noun_phrase(L1,L2,Object,C1,C2).
mp(L,L,_,C,C).

% DICTIONARY
% adj(L0,L1,Entity,C0,C1) is true if L0-L1
% is an adjective that imposes constraints C0-C1 Entity
adj([left | L],L,Entity, [left(Entity)|C],C).
adj([center | L],L,Entity, [center(Entity)|C],C).
adj([right | L],L,Entity, [right(Entity)|C],C).

noun([party | L],L,Entity, [party(Entity)|C],C).
noun([leader | L], L, Entity, [leader(Entity) |C], C).
noun([policy | L], L, Entity, [policy(Entity) |C], C).

% Parties are proper nouns.
% We could either have it check a language dictionary or add the constraints. We chose to check the dictionary.
proper_noun([X | L],L,X,C,C) :- party(X).
proper_noun([X | L],L,X,C,C) :- policy(X).

reln([the,leader,of| L],L,O1,O2,[leaderOf(O2,O1)|C],C).
reln([supports | L],L,O1,O2,[supports(O2,O1)|C],C).


% question(Question,QR,Entity) is true if Query provides an answer about Entity to Question
question(['Does' | L0],L2,Entity,C0,C2) :-
    noun_phrase(L0,L1,Entity,C0,C1),
    mp(L1,L2,Entity,C1,C2).
question(['Is' | L0],L2,Entity,C0,C2) :-
    noun_phrase(L0,L1,Entity,C0,C1),
    mp(L1,L2,Entity,C1,C2).
question(['What',is | L0], L1, Entity,C0,C1) :-
    mp(L0,L1,Entity,C0,C1).
question(['What',is | L0],L1,Entity,C0,C1) :-
    noun_phrase(L0,L1,Entity,C0,C1).
question(['What' | L0],L2,Entity,C0,C2) :-
    noun_phrase(L0,L1,Entity,C0,C1),
    mp(L1,L2,Entity,C1,C2).
question(['Who', is |L0], L1,Entity,C0,C1) :-
    noun_phrase(L0,L1,Entity,C0,C1).
question(['Who', is |L0], L1,Entity,C0,C1) :-
        mp(L0,L1,Entity,C0,C1).
question(['Who' | L0],L2,Entity,C0,C2) :-
        noun_phrase(L0,L1,Entity,C0,C1),
        mp(L1,L2,Entity,C1,C2).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    get_constraints_from_question(Q,A,C),
    prove_all(C).

% get_constraints_from_question(Q,A,C) is true if C is the constaints on A to infer question Q
get_constraints_from_question(Q,A,C) :-
    question(Q,End,A,C,[]),
    member(End,[[],['?'],['.']]).


% prove_all(L) is true if all elements of L can be proved from the knowledge base
prove_all([]).
prove_all([H|T]) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(T).

%  The Database of Facts to be Queried

% party(P) is true if P is a party.
party(liberals).
party(conservatives).
party(ndp).
party(greens).
party(bq).
party(rhinoceros).

% leader(L) is true if L is a leader.
leader(X) :- leaderOf(P,X), party(P).

% left(X) is true if X is more politically left aligned.
left(ndp).
left(greens).
left(X) :- leaderOf(P,X), left(P).

% center(X) is true if X is more politically center aligned.
center(liberals).
center(bq).
center(X) :- leaderOf(X,P),  center(P).

% right(X) is true if X is more politically right aligned.
right(conservatives).
right(X) :- leaderOf(X,P), right(P).

% leaderOf(P,L) is true if L is the leader of party P.
leaderOf(liberals, trudeau).
leaderOf(conservatives, scheer).
leaderOf(ndp, singh).
leaderOf(greens, may).
leaderOf(bq, blanchet).

% policy(P) is true if P is a policy that a party could support or oppose
policy('deregulation').
policy('pharmacare').
policy('tax cuts').
policy('carbon tax').
policy('wealth tax').
policy('bike lanes').
policy('foreign military intervention').
policy('unions').
policy(abortion).
policy('repealing the law of gravity').
policy('quebec nationalism').

% supported-by(P1,P2) is true if P1 is a policy that is supported by P2
supports('deregulation',conservatives).
supports('deregulation',liberals).
supports('pharmacare',ndp).
supports('pharmacare',greens).
supports('tax cuts',conservatives).
supports('tax cuts',liberals).
supports('tax cuts',ndp).
supports('tax cuts',greens).
supports('tax cuts',bq).
supports('carbon tax',liberals).
supports('carbon tax',greens).
supports('carbon tax',ndp).
supports('carbon tax',bq).
supports('wealth tax',ndp).
supports('bike lanes',liberals).
supports('bike lanes',greens).
supports('bike lanes',ndp).
supports('bike lanes',bq).
supports('foreign military intervention',conservatives).
supports('foreign military intervention',liberals).
supports('unions',ndp).
supports('abortion',liberals).
supports('abortion',greens).
supports('abortion',ndp).
supports('abortion',bq).
supports('quebec nationalism',bq).
supports('repealing the law of gravity',rhinoceros).


q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    readln(Ln),
    ask(Ln,Ans).


/* Try the following queries:
ask(['What',is,a,right,party],A).
ask(['Who',is,a,leader],A).
ask(['Who',is,the,leader,of,a,left,party],A).
ask(['Who',is,the,leader,of,liberals],A).
ask(['What',is,a,party,that,supports,'pharmacare'],A).

*/
