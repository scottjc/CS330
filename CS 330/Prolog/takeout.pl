member(X, [X|R]).
member(X, [Y|R]) :- member(X,R).


takeout(X, [X|R], R).
takeout(X, [Y|R],[Y|Z]) :- takeout (X,R,Z).