%%%%%%%%% Morning Dress-Up %%%%%%%%%

%  Weather conditions
weather(rainy). 
weather(sunny).
weather(cloudy). 
weather(snowy). 
weather(highWind). 
weather(humid).

%  Mood conditions
mood(happy).
mood(tired). 
mood(sad).
mood(sexy).
mood(indifferent).

%  Activity conditions
activity(regularDay).
activity(busyDay).
activity(funDay).
activity(bumDay).

%  Tops options
tops(t-shirt).
tops(buttonedTop).
tops(tankTop).
tops(hoodie).
tops(niceJacket).
tops(coat).

%  Bottoms options
bottoms(jeans). 
bottoms(shorts). 
bottoms(suitPants). 
bottoms(sweatpants). 
bottoms(khakis). 
bottoms(skirt).
bottoms(leggings).

%  Shoes options
shoes(sneakers).
shoes(dressShoes).
shoes(boots). 
shoes(sandals). 
shoes(heels). 
shoes(slippers).

% Condition for the day
condition(A) :- 
    %write('Weather: '), write(sunny), nl, 
    %write('Mood: '), write(happy), nl,
    %write('Activity: '), write(funDay), nl.
	A \= 'sunny, happy, funDay', 
    A \= 'cloudy, tired, bumDay',
    A \= 'snowy,indifferent, ', 
    write('Not available condition').
condition(A) :-
   (   A = 'sunny, happy, funDay'->
          write('Top: t-shirt'), nl,
          write('Bottoms: Jeans'), nl,
          write('Shoes: Sneakers'), nl,
       write('Is the outfit option acceptable? :'), nl,
      ( read(x),
       x = 'yes' -> fail
   ;   x = 'no' -> write(''))          
   ;   A = 'cloudy, tired, bumDay').
%random_condition(weather, mood, activity) :-
%    condition(sunny, happy, funDay),
%    condition(cloudy, tired, bumDay),
%    condition(snowy, indifferent, regularDay).
%   Outfit Suggestion
option :-
		
     all_different([tops, bottoms, shoes]),
    
     all_different([weather, mood, activity]),
          
% clothes for weather condition 
sunny(t-shirt, buttonedTop, tankTop, hoodie, niceJacket, jeans, shorts, suitPants, khakis, skirt, sneakers, dressShoes, sandals, heels, slippers).
cloudy(t-shirt, buttonedTop, hoodie, coat, jeans, suitPants, sweatpants, khakis, skirt, leggings, sneakers, dressShoes, sandals, heels, slippers).
rainy(hoodie, coat, sweatpants, leggings, sneakers, boots).
snowy(coat, sweatpants, khakis, leggings, boots).
highWind(buttonedTop, coat, jeans, sweatpants, leggings, boots).
humid(tankTop, shorts, skirt, sandals, slippers).

%clothes based on mood
happy(t-shirt, buttonedTop, niceJacket, coat, jeans, shorts, suitPants, khakis, skirt, sneakers, dressShoes, sandals, heels, slippers).
tired(t-shirt, tankTop, hoodie, coat, shorts, sweatpants, leggings, sneakers, boots, slippers). 
sad(hoodie, coat, sweatpants,leggings).
sexy(buttonedTop, niceJacket, suitPants, khakis, skirt, dressShoes, heels).
indifferent(t-shirt, tankTop, hoodie, coat, jeans, shorts, sweatpants, leggings, sneakers, boots, sandals, slippers).

%clothes based on activity
regularDay(t-shirt, buttonedTop, tankTop, hoodie, niceJacket, coat, jeans, shorts, sweatpants, skirt, leggings, sneakers, boots, sandals, slippers).
busyDay(buttonedTop, niceJacket,suitPants, khakis, dressShoes, heels).
funDay(t-shirt, hoodie, jeans, shorts,suitPants, khakis, skirt, sneakers, dressShoes, sandals, heels, slippers).
bumDay(t-shirt, tankTop, hoodie, coat, shorts, sweatpants, leggings, sneakers, boots, slippers).

% N is any weather/activity/mood and X is any item in a certain category 
greater(N, [X], 1).

begin:- loop(A).   
    loop(A) :-
    write('The condition for the day: '),nl,
    write('Weather: '), write('rainy'), nl,      %write('cloudy') 
    write('Mood: '), write('lazy'), nl,         %write('indifferent')
    write('Activity: '), write('regularDay'), nl,  
    write('Option: '), nl,
    write('Top: coat'), nl,
    write('Bottoms: sweatpants'), nl,
    write('Shoes: sneakers'), nl,
    write('Is the outfit option acceptable? :'), 
    read(A), nl,  
    write('What did you not like about it?: '), 
    read(A), nl,
    write('How about this?: '), nl,
    write('Option: '), nl,
    write('Top: hoodie'), nl,
    write('Bottoms: sweatpants'), nl,
    write('Shoes: sneakers'), nl,
    write('Is the outfit option acceptable?: '),
    read(A), nl,
    write('What did you not like about it?: '), 
    read(A), nl,
    write('How about this?: '), nl,
    write('Option: '), nl,
    write('Top: hoodie'), nl,
    write('Bottoms: leggings'), nl,
    write('Shoes: sneakers'), nl,
    write('Is the outfit option acceptable?: '),
    read(A), nl,
    write('What did you not like about it?: '), 
    read(A), nl,
    write('How about this?: '), nl,
    write('Option: '), nl,
    write('Top: coat'), nl,
    write('Bottoms: sweatpants'), nl,
    write('Shoes: boots'), nl,
    write('Is the outfit option acceptable?: '),
    read(A), write(A), nl, (A=yes; begin).  
	%;   A = end, fail.    
  

% first
first([H|_],H).
%  rest
rest([_|T], T).
% last element
last([H|[]],H).
last([_|T], Result) :- last(T, Result).
% nth element
nth(0, [H|_],H).
nth(N,[_|T],E) :- K is N - 1, nth(K,T,E).

% list.
writelist([]).
writelist([H|T]) :- write(H), nl, writelist(T).
add_last(X, [H|T], [H|TX]) :- add_last(X, T, TX).

% pick(List,Name)
pick(L,Item) :-
length(L,Length),
random(0,Length,RN),
nth(RN,L,Item).

% makes set of 
make_set([],[]).
make_set([H|T],TS) :-
member(H,T),
make_set(T,TS).
make_set([H|T],[H|TS]) :-
make_set(T,TS).

%List of Num size.
make_list(0,_,[]).
make_list(Num,Element,Name) :-
 K is Num - 1,
 make_list(K,Element,NameK),
 add_last(Element,NameK,Name).
% rdc of list
but_first([],[]).
but_first([_],[]).
but_first([_|N],N).
% rac of list
but_last([],[]).
but_last([_],[]).
but_last([H|T], Name) :-
 reverse(T, [_|B]), reverse(B, RDC), add_first(H,RDC,Name).