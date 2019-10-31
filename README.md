SCS 3547 Assignment 2
=====================

Authors: Shaoqing Tan, Shihan Sun

## Overview
The program is using the prolog language like a procedural language, which in our oppinion defeats the purpose of prolog. The word routine is used to refer to these procedural predicates.

## Start: Kick Start Routine
```prolog
start :-
    format('Initializing started...~n', []),
    init,
    format('Let the game begin!~n', []),
    take_steps([[1,1]]).
```

This is the entrypoint of the program. It calls init and then kick start the iterations with a list of the starting position (1,1)

## Init: Create the world
```prolog
init :-
    init_game,
    init_land_fig72,
    init_agent,
    init_wumpus.
```

The init routine is a umbrella for initializing different aspects of the game.

### Init Game
```prolog
init_game :-
    retractall( time_taken(_) ),
    assert( time_taken(0) ),

    retractall( score(_) ),
    assert( score(0) ),

    retractall( visited(_) ),
    assert( visited(1) ),

    retractall( isWumpus(_,_) ),
    retractall( isGold(_,_) ),

    retractall( visited_cells(_) ),
    assert( visited_cells([]) ).
```

This routine is the first of many with the same pattern, which is using a fact as a variable in the procedural sense and reassigning values to it by retracting all facts of the same kind by calling `retractall` with `_` as argument and inserting a new one. We see here we are initializing 4 facts `time_taken`, `score`, `visited`, and `visited_cells` in this way which acts as variables. (We can later find out `visited` is not useful)

`isWumpus` and `isGold` are used slightly differently in a sense that they contain multiple entries of its type and are partially retracted. Their values are cleared out during init.

### Dynamic
All retractable facts here must be declared `dynamic` at the top of the file, i.e.
```prolog
:- dynamic ([
	     agent_location/1,
	     gold_location/1,
	     pit_location/1,
	     time_taken/1,
	     score/1,
	     visited/1,
	     visited_cells/1,
	     world_size/1,
	     wumpus_location/1,
             isPit/2,
             isWumpus/2,
             isGold/2
	    ]).
```

### Init Map
```prolog
init_land_fig72 :-
    retractall( world_size(_) ),
    assert( world_size(4) ),

    retractall( gold_location(_) ),
    assert( gold_location([3,2]) ),

    retractall( pit_location(_) ),
    assert( pit_location([4,4]) ),
    assert( pit_location([3,3]) ),
    assert( pit_location([1,3]) ).
```

This routine assigns facts `world_size` to 4 and `gold_location` to list [3,2], and 3 `pit_locations` as [4,4], [3,3], [1,3]. All these facts should be constant across the game.

### Init Agent
```prolog
init_agent :-
    retractall( agent_location(_) ),
    assert( agent_location([1,1]) ),

    visit([1,1]).
```

This routine initializes fact `agent_location` to [1,1], and calls the `visit` routine.

### Visit
```prolog
visit(Xs) :-
    visited_cells(Ys),
    retractall( visited_cells(_) ),
    assert( visited_cells([Ys|Xs]) ).
```

This routine
1. Solves the value of `visited_cells` fact to a varialbe `Ys`.
2. Clears `visited_cells`.
3. Reassigns `visited_cells` as a the concatenation of the old list `Ys` and new item `Xs`.

### Init Wumpus
```prolog
init_wumpus :-
    retractall( wumpus_location(_) ),
    assert( wumpus_location([4,1]) ).
```

This routine initializes fact `wumpus_location` to [4,1].

## Taking Steps
```prolog
take_steps(VisitedList) :-
    make_percept_sentence(Perception),
    agent_location(AL),
    format('I\'m in ~p, seeing: ~p~n', [AL,Perception]),

    update_KB(Perception),
    ask_KB(VisitedList, Action),
    format('I\'m going to: ~p~n', [Action]),

    update_time,
    update_score,

    agent_location(Aloc),
    VL = [Aloc|VisitedList],
    standing,
    step_pre(VL).
```

`take_steps` is called with initial value of [1,1] after init. This routine does the following

1. Calculate perceptions with `make_percept_sentence`.
2. Update knowledge base with `update_KB`.
3. Calculate the next move with `ask_KB`.
4. Update time and score.
5. Appends the decision to `VisitedList` or `VL`.
6. Check for end game with `standing` and `step_pre`.
7. Repeat by recursing through the `step_pre` routine.

### Perception
```prolog
make_percept_sentence([Stench,Bleeze,Glitter]) :-
	smelly(Stench),
	bleezy(Bleeze),
	glittering(Glitter).
```

This routine is a umbrella for three different perceptions.

```prolog
adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).

adjacent( [X1, Y1], [X2, Y2] ) :-
    ( X1 = X2, adj( Y1, Y2 )
    ; Y1 = Y2, adj( X1, X2 )
    ).

isSmelly(Ls1) :-
    wumpus_location( Ls2 ),
    adjacent( Ls1, Ls2 ).

smelly(yes) :-
    agent_location(AL),
    isSmelly(AL).
smelly(no).
```

To find out if there is a stench, the `smelly` predicate is used, which relies on the `isSmelly` and `adjacent` predicate. This is the part of the code that exercises the querying power of prolog.

* `adj` defineds facts of relations between two numbers to produce if they are adjacent.
* `adjacent` uses `adj` to defined relations between two points to produce adjacency.
* `isSmelly` is a clause that relies on solving for a location `Ls2` which is both the `wumpus_location` and also `adjacent` to the given target location `Ls1`.
* `smelly` is defined with careful order. At first if solves for if `agent_location` AL is also smelly, if so, a `yes` is produced. But if the first clause cannot be proven, there is a second constant clause that evaluates to `no`, which makes up the rest of the space, meaning there will always be a value for smelly. Note, reverse the ordering of the two lines will break the logic.

```prolog
isBleezy(Ls1) :-
    pit_location( Ls2 ),
    adjacent( Ls1, Ls2 ).

bleezy(yes) :-
    agent_location(AL),
    isBleezy(AL).
bleezy(no).
```

Bleezy works very similar to stench, difference being it uses `pit_location`.

```prolog
isGlittering( [X1, Y1] ) :-
    gold_location( [X2, Y2] ),
    X1 = X2,
    Y1 = Y2.

glittering(yes) :-
    agent_location(AL),
    isGlittering(AL).
glittering(no).
```

Glittering also works very similar to stench, the difference being it uses perfect coincidency with `gold_location`.

### Updating Knowledge base

```prolog
update_KB( [Stench,Bleeze,Glitter] ) :-
    add_wumpus_KB(Stench),
    add_pit_KB(Bleeze),
    add_gold_KB(Glitter).
```

This routine umbrellas three other routines and passes perceptions.

```prolog
add_pit_KB(no) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).

add_pit_KB(yes) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).

assume_pit(no, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(no, L) ),
    format('KB learn ~p - there\'s no Pit there!~n', [L]).

assume_pit(yes, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(yes, L) ),
    format('KB learn ~p - its a Pit!~n', [L]).
```

This routine updates `isPit` facts based on the agent perception.

* `assume_pit` takes either `yes` or `no` as the first argument, and retracts all `isPit` entries on the given point `L`, then assigns `isPit` to align with the first argument.
* add_pit_KB also takes a `yes` or `no` as first argument, then calls `assume_pit` on the surroundings of the `agent_location` [X,Y] using intermediate variables `Z1`, `Z2`, `Z3`, `Z4`.

```prolog
add_wumpus_KB(no) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_wumpus(no,[X,Z1]),
    Z2 is Y-1, assume_wumpus(no,[X,Z2]),
    Z3 is X+1, assume_wumpus(no,[Z3,Y]),
    Z4 is X-1, assume_wumpus(no,[Z4,Y]).

assume_wumpus(no, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(no, L) ),
    format('KB learn ~p - no Wumpus there!~n', [L]).

assume_wumpus(yes, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(yes, L) ),
    format('KB learn ~p - possibly the Wumpus is there!~n', [L]).
```

This routine updates the `isWumpus` fact very similarly to the above `isPit` version with only the perceptions replaced.

```prolog
add_gold_KB(no) :-
    gold_location(GL),
    assume_gold(no, GL).

add_gold_KB(yes) :-
    gold_location([X1,Y1]),
    agent_location([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).

assume_gold(no, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(no, L) ),
    format('KB learn ~p - there\'s no gold here!~n', [L]).

assume_gold(yes, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(yes, L) ),
    format('KB learn ~p - GOT THE GOLD!!!~n', [L]).
```

This routine updates the `isGold` fact very similarly to the above `isPit` version with perceptions replaced and it uses coincidental instead of adjacent location.

### Asking Knowledge Base

```prolog
ask_KB(VisitedList, Action) :-
    isWumpus(no, L),
    isPit(no, L),
    permitted(L),
    not_member(L, VisitedList),
    update_agent_location(L),
    Action = L.

permitted([X,Y]) :-
    world_size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.

not_member(_, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).

update_agent_location(NewAL) :-
    retractall( agent_location(_) ),
    assert( agent_location(NewAL) ).
```

This routines solves for a new agent location `L` or `Action` that satisfies the following

* There is no wumpus by querying against `isWumpus`.
* There is no pit, by querying against `isPit`.
* It is inside the map (`permitted`) by limiting variable values with world size.
* It is not a member of the `VisitedList`, by using recursive routine `not_member` to break apart of the list. Note the termination for recursion, everything is considered not a member of empty list.

Then it reassigns the `agent_location` fact with `update_agent_location`.

### Updating Time and Score

```prolog
update_time :-
    time_taken(T),
    NewTime is T+1,
    retractall( time_taken(_) ),
    assert( time_taken(NewTime) ).

update_score :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    update_score(AL, GL, WL).

update_score(P) :-
    score(S),
    NewScore is S+P,
    retractall( score(_) ),
    assert( score(NewScore) ).

update_score(AL, AL, _) :-
    update_score(1000).

update_score(_,_,_) :-
    update_score(-1).
```

In this routine, `time_taken` fact is reassigned and incremented.

`score` fact is updated in the following cases based on agent, gold and wumpus locations

* Agent and gold are in the same location - increase score by 1000.
* Any other case - decrease score by 1.

### End Game

```prolog
step_pre(VisitedList) :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    score(S),
    time_taken(T),

    ( AL=GL -> writeln('WON!'), format('Score: ~p,~n Time: ~p', [S,T])
    ; AL=WL -> format('Lost: Wumpus eats you!~n', []),
               format('Score: ~p,~n Time: ~p', [S,T])
    ; take_steps(VisitedList)
    ).

standing :-
    wumpus_location(WL),
    gold_location(GL),
    agent_location(AL),

    ( is_pit(yes, AL) -> format('Agent was fallen into a pit!~n', []),
      fail
    ; stnd(AL, GL, WL)
    ).

stnd(_, _, _) :-
    format('There\'s still something to do...~n', []).

stnd(AL, _, AL) :-
    format('YIKES! You\'re eaten by the wumpus!', []),
    fail.

stnd(AL, AL, _) :-
    format('AGENT FOUND THE GOLD!!', []),
    true.
```

The `standing` routine makes use of the clauses defined as `stnd` to detect the following end game

* Agent and wumpus are in same location - failure and game (`start` routine) finishes with `false`.
* Other cases - continue.

 Immediately after, in `step_pre`, locations are checked again

* Agent and gold are in the same location - success and quit the game with `true`.
* Agent and wumpus are in same location - failure. (redundant since we already quit the game in `standing`)
* Other cases - continue by calling `take_steps`.

During `ask_KB` if a new `agent_location` cannot be solved, the game also ends with `false`.

## Comments

This implementation of the wumpus world differs from the class in the following ways

1. The agent is allowed to jump.
2. The agent has no direction.
3. The agent has no arrow.
4. The pit / wumpus location deduction logic is flawed.

## Sample Printout

```
Initializing started...
Let the game begin!
I'm in [1,1], seeing: [no,no,no]
KB learn [1,2] - no Wumpus there!
KB learn [1,0] - no Wumpus there!
KB learn [2,1] - no Wumpus there!
KB learn [0,1] - no Wumpus there!
KB learn [1,2] - there's no Pit there!
KB learn [1,0] - there's no Pit there!
KB learn [2,1] - there's no Pit there!
KB learn [0,1] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [1,2]
There's still something to do...
I'm in [1,2], seeing: [no,yes,no]
KB learn [1,3] - no Wumpus there!
KB learn [1,1] - no Wumpus there!
KB learn [2,2] - no Wumpus there!
KB learn [0,2] - no Wumpus there!
KB learn [1,3] - its a Pit!
KB learn [1,1] - its a Pit!
KB learn [2,2] - its a Pit!
KB learn [0,2] - its a Pit!
KB learn [3,2] - there's no gold here!
I'm going to: [2,1]
There's still something to do...
I'm in [2,1], seeing: [no,no,no]
KB learn [2,2] - no Wumpus there!
KB learn [2,0] - no Wumpus there!
KB learn [3,1] - no Wumpus there!
KB learn [1,1] - no Wumpus there!
KB learn [2,2] - there's no Pit there!
KB learn [2,0] - there's no Pit there!
KB learn [3,1] - there's no Pit there!
KB learn [1,1] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [2,2]
There's still something to do...
I'm in [2,2], seeing: [no,no,no]
KB learn [2,3] - no Wumpus there!
KB learn [2,1] - no Wumpus there!
KB learn [3,2] - no Wumpus there!
KB learn [1,2] - no Wumpus there!
KB learn [2,3] - there's no Pit there!
KB learn [2,1] - there's no Pit there!
KB learn [3,2] - there's no Pit there!
KB learn [1,2] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [3,1]
There's still something to do...
I'm in [3,1], seeing: [yes,no,no]
I'm in [3,1], seeing: [no,no,no]
KB learn [3,2] - no Wumpus there!
KB learn [3,0] - no Wumpus there!
KB learn [4,1] - no Wumpus there!
KB learn [2,1] - no Wumpus there!
KB learn [3,2] - there's no Pit there!
KB learn [3,0] - there's no Pit there!
KB learn [4,1] - there's no Pit there!
KB learn [2,1] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [2,3]
There's still something to do...
I'm in [2,3], seeing: [no,yes,no]
KB learn [2,4] - no Wumpus there!
KB learn [2,2] - no Wumpus there!
KB learn [3,3] - no Wumpus there!
KB learn [1,3] - no Wumpus there!
KB learn [2,4] - its a Pit!
KB learn [2,2] - its a Pit!
KB learn [3,3] - its a Pit!
KB learn [1,3] - its a Pit!
KB learn [3,2] - there's no gold here!
I'm going to: [3,2]
There's still something to do...
WON!
Score: 995,
 Time: 6
true
```
