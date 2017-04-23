% Problem #1, "School's Out!"
% Each teacher is teaching a different subject, is going to a different state, and doing something different.

teacher(msappleton).
teacher(msgross).
teacher(mrknight).
teacher(mrmcevoy).
teacher(mssparnell).

subject(english).
subject(gym).
subject(history).
subject(math).
subject(science).

state(california).
state(florida).
state(maine).
state(oregon).
state(virginia).

activity(antiquing).
activity(camping).
activity(sightseeing).
activity(splunking).
activity(water-skiing).
 
solve :-
    teacher(msappleton), teacher(msgross), teacher(mrknight), teacher(mrmcevoy), teacher(mssparnell),
    all_different([appletonsubject, grosssubject, knightsubject, mcevoysubject, parnellsubject]),
    all_different([appletonstate, grossstate, knightstate, mcevoystate, parnellstate]),
    all_different(appletonactivity, grossactivity, knightactivity, mcevoyactivity, parnellactivity]),

    %need some related things here

    %each list is a quad [teacher, subject, state, activity]
    Quads = [   [msappleton, appletonsubject, appletonstate, appletonactivity],
                [msgross, grosssubject, grossstate, grossactivity],
                [mrknight, knightsubject, knightstate, knightactivity],
                [mrmcevoy, mcevoysubject, mcevoystate, mcevoyactivity],
		[mssparnell, parnellsubject, parnellstate, parnellactivity]],
 
  %Story detail rules here
    % 1. Ms. Gross teachers either math or science. If she is going antiquing,
    %    then she is going to florida; otherwise, she is going to California.
    (member([msgross, math, _, _], Quads) ;
        member([msgross, science, _, _], Quads)),

    ( (member([msgross, _, _, antiquing], Quads) ,
        member([msgross, _, florida, _], Quads));
       (member([msgross, _, _, _], Quads)	   ),
 

    % 2. The science teacher (who is going water-skiing) is going to go to Cali
    % or florida. Mr. McEvoy (who is the History teacher) is going to either Maine or Oregon.
    ( (member([_, science, _, water-skiing], Quads),
	( (member([_, science, california, _], Quads) ;
         (member([_, science, florida, _], Quads) ) ),

     member([mrmcevoy, history, _, _], Quads),

    ( member([mrmcevoy, _, maine, _], Quads) ;
        member([mrmcevoy, _, oregon, _], Quads) ),


    % 3. If the woman who is going to virgina is the English teacher, then it is
    % Ms. Appleton; otherwise she is Ms.Parnell (who is going spelunking).
    (member([msappleton, _, virginia, _], Quads) ;
     member([msgross, _, virginia, _], Quads) ;
     member([mssparnell, virginia, _, _], Quads)),

    member([msparnell, _, _, splunking], Quads),



 
    tell([msappleton, appletonsubject, appletonstate, appletonactivity]),
    tell([msgross, grosssubject, grossstate, grossactivity]),
    tell([mrknight, knightsubject, knightstate, knightactivity]),
    tell([mrmcevoy, mcevoysubject, mcevoystate, mcevoyactivity]),
    tell([mssparnell, parnellsubject, parnellstate, parnellactivity]).	
 
% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).
 
tell(W, X, Y, Z) :-
    write(W), write(', who teaches '), write(X), write(', went to '), 
    write(Y), write(' and did some'), write(Z), write('.'), nl.