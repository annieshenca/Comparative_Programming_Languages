% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $

%
% Annie Shen (ashen7)
% CMPS 112 - Mackey
% Asg 4 - Prolog Airline Reservation System


%
% ***************************************************************************
% Provide result for the trip the user asked for.
fly(D, A) :-
    % printing out the flight info of departure and arrival
    airport (D, _, _, _ ),
    airport (A, _, _, _ ),
    % Shows the paths in case where there is multiple desparts and arrivals.
    flightPath (D, A, [D], FLIGHT_PLAN, _),
    !,
    nl,
    writePath (FLIGHT_PLAN),
    true.

%
% ***************************************************************************
% Error checkings. In case when the user is not being so smart.
% If user try to search the same place as both departure AND arrival... Why though?
fly (D, D) :-
    format('ERROR: No trip for path that the start and finish is the same place.'),
    nl,
    !,
    fail.

%
% If user asked for a path that does not exist.
fly (D, A) :-
    airport(D, _, _, _ ),
    airport(A, _, _, _ ),
    format('ERROR: There is no flights available for this path you are searching'),
    !,
    fail.

%
% If the user gave no departure and arrival places. Of course fail.
fly (_, _) :-
    !,
    nl,
    fail.


%
% ***************************************************************************
% Converts degrees and minutes into radians like the PDF liked it
toRadians (DEGREES, MINUTES, RADIANS) :-
    X is DEGREES + MINUTES / 60,
    RADIANS is X * pi / 180.

%
% Converts some miles to FLIGHT_X hours based on 500mph avg. flight plane speed.
hoursFromMiles(MILES, HOURS) :-
    HOURS is MILES / 500.

%
% Grabs coordinates from airports and returns distance
parseLatToLong (CODE1, CODE2, DIST) :-
	airport   (CODE1, _, degmin(DEG_LAT1, M1), degmin(DEG_LONG1, M2)),
	airport   (CODE2, _, degmin(DEG_LAT2, M3), degmin(DEG_LONG2, M4)),
	toRadians (DEG_LAT1,  M1, LAT1),
	toRadians (DEG_LAT2,  M3, LAT2),
	toRadians (DEG_LONG1, M2, LONG1),
	toRadians (DEG_LONG2, M4, LONG2),
	haversineRadians (LAT1, LONG1, LAT2, LONG2, DIST).

%
% Provided from Professor Mackeys website, in file functions.pl
haversineRadians (Lat1, Lon1, Lat2, Lon2, Dist) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin(Dlat / 2) ** 2
      + cos(Lat1) * cos(Lat2) * sin(Dlon / 2) ** 2,
   D is 2 * atan2(sqrt(A), sqrt(1 - A)),
   Dist is D * 3961.

%
% Converts hours and minutes into just hours.
toHours (time(HOUR,MIN), HOURS) :-
    HOURS is HOUR + MIN / 60.

%
% Print the time variable.
printTime (TOTAL_HOURS) :-
    TOTAL_MINUTES is floor(TOTAL_HOURS * 60 ),
    HOURS is TOTAL_MINUTES // 60,
    MINUTES is TOTAL_MINUTES mod 60,
    print_digits(HOURS),
    write(':'),
    print_digits(MINUTES).

    % Prints time when time is in hte formate of 01:00.
	printDigits (FULL_TIME) :-
    	FULL_TIME < 10,
    	format('0~w', [FULL_TIME]).

	% Prints time when time is in the formate of 23:00.
	printDigits (FULL_TIME) :-
    	FULL_TIME >= 10,
    	print(FULL_TIME).


%
% ***************************************************************************
% Generates a flight plan from ARRIVAL -> DEST keeps track of LEG(s) of trip
% Prints every leg till DEST, while ensuring trip is not longer than 24 hrs

flightPath (AIRPORT, AIRPORT, _, [AIRPORT], _).

flightPath (PREVIOUS_AIRPORT, AIRPORT, LEG, [[PREVIOUS_AIRPORT, DEPATURE_TIME, ARRIVAL_TIME] | CLIST], DEPARTURE_TIME_RAW) :-
    flight (PREVIOUS_AIRPORT, AIRPORT, DEPARTURE_TIME_RAW),
    not (member(AIRPORT, LEG)),
    toHours (DEPARTURE_TIME_RAW, DEPATURE_TIME),
    parseLatLong (PREVIOUS_AIRPORT, AIRPORT, FLIGHT_DISTANCE),
    hoursFromMiles (FLIGHT_DISTANCE, FLIGHT_TIME),
    ARRIVAL_TIME is DEPATURE_TIME + FLIGHT_TIME,
    ARRIVAL_TIME < 24.0,
    flight_path(AIRPORT, AIRPORT, [AIRPORT | LEG], CLIST, _).

flightPath (PREVIOUS_AIRPORT, AIRPORT, LEG, [[PREVIOUS_AIRPORT, DEPATURE_TIME, ARRIVAL_TIME] | CLIST], DEPARTURE_TIME_RAW ) :-
    flight (PREVIOUS_AIRPORT, DEST_AIRPORT, DEPARTURE_TIME_RAW),
    not (member(DEST_AIRPORT, LEG)),
    toHours (DEPARTURE_TIME_RAW, DEPATURE_TIME),
    parseLatLong (PREVIOUS_AIRPORT, DEST_AIRPORT, FLIGHT_DISTANCE),
    hoursFromMiles (FLIGHT_DISTANCE, FLIGHT_TIME),
    flight (DEST_AIRPORT, _, NEXT_FLIGHT_TIME_RAW),
    toHours (NEXT_FLIGHT_TIME_RAW, NEXT_FLIGHT_DEPART_TIME),
    ARRIVAL_TIME is DEPATURE_TIME + FLIGHT_TIME,
    LAYOVER is NEXT_FLIGHT_DEPART_TIME - ARRIVAL_TIME - 0.5,
    LAYOVER >= 0,
    ARRIVAL_TIME < 24.0,
    flight_path (DEST_AIRPORT, AIRPORT, [DEST_AIRPORT | LEG], CLIST, NEXT_FLIGHT_TIME_RAW).


%
% ***************************************************************************
% Helper function Not. Found in Processor s example file graphpaths.pl
not (X) :- X, !, fail.
% Should not be used? But in case if passing in nothing then nothing happens.
not (_).


%
% ***************************************************************************
% Function to print out the flight path.
% if the function was called without any arguments, return nothing.
writePath ([]) :-
    nl.

writePath ([[FLIGHT_X, FLIGHT_X_DEPART_TIME, FLIGHT_X_ARRIVAL_TIME], FLIGHT_Y | []] ) :-
    airport (FLIGHT_X, FLIGHT_X_TIME, _, _),
    airport (FLIGHT_Y, FLIGHT_Y_TIME, _, _),
    format ('    depart ~w  ~w', [FLIGHT_X,FLIGHT_X_TIME]),
    (print_time(FLIGHT_X_DEPART_TIME)),
    nl,
    format ('    arrive ~w  ~w', [FLIGHT_Y,FLIGHT_Y_TIME]),
    (print_time(FLIGHT_X_ARRIVAL_TIME)),
    nl,
    !,
    true.

writePath ([[FLIGHT_X, FLIGHT_X_DEPART_TIME, FLIGHT_X_ARRIVAL_TIME], [FLIGHT_Y, FLIGHT_Y_DEPART_TIME, FLIGHT_Y_ARRIVAL_TIME] | CLIST]) :-
    airport (FLIGHT_X, FLIGHT_X_TIME, _, _),
    airport (FLIGHT_Y, FLIGHT_Y_TIME, _, _),
    format('    depart ~w  ~w', [FLIGHT_X,FLIGHT_X_TIME]),
    (print_time(FLIGHT_X_DEPART_TIME)),
    nl,
    format('    arrive ~w  ~w', [FLIGHT_Y,FLIGHT_Y_TIME]),
    (print_time(FLIGHT_X_ARRIVAL_TIME)),
    nl,
    !,
    writePath ([[FLIGHT_Y, FLIGHT_Y_DEPART_TIME, FLIGHT_Y_ARRIVAL_TIME] | CLIST]).
