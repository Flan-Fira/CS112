not(X) :- X, !, fail.
not(_).

% convert Deg Min to Deg only
convert_deg_to_min( degmin( Deg, Min ), DegOnly ) :-
   DegOnly is Deg + Min / 60.

% calculate distance between two points
cal_dis( X1, Y1, X2, Y2, Hypotenuse ) :-
   DelY is Y1 - Y2,
   DelX is X1 - X2,
   Hypotenuse is sqrt( DelX * DelX + DelY * DelY ).

% calculate distance between two airports
airports_dist( Location1, Location2, DistanceMiles ) :-
   airport( Location1, _, Latitude1, Longitude1 ),
   airport( Location2, _, Latitude2, Longitude2 ),
   convert_deg_to_min( Latitude1, Latdegrees1 ),
   convert_deg_to_min( Latitude2, Latdegrees2 ),
   convert_deg_to_min( Longitude1, Longdegrees1 ),
   convert_deg_to_min( Longitude2, Longdegrees2 ),
   cal_dis( Latdegrees1, Longdegrees1, Latdegrees2, Longdegrees2,
               DistanceDegrees ),
   DistanceMiles is DistanceDegrees * 69.

fly_time(Location1, Location2, FlightTime) :-
   airports_dist(Location1, Location2, DistanceMiles),
   FlightTime is DistanceMiles / 500.

arrival_time(flight(Location1, Location2, time(DH,DM)), ArrivalTime) :-
   fly_time(Location1, Location2, FlightTime),
   convert_hour(time(DH,DM), DepartureTime),
   ArrivalTime is DepartureTime + FlightTime. % Unit is HourOnly

% convert Min to hour
convert_minute(Mins, Hours) :-
   Mins is Hours * 60.

% convert to hour only
convert_hour( time( Hours, Mins) , HourOnly ) :-
   HourOnly is Hours + Mins / 60.

% get two digits for time
print_digits_of_time( Digits ) :-
   Digits < 10, print( 0 ), print( Digits ).

print_digits_of_time( Digits ) :-
   Digits >= 10, print( Digits ).

% print schedule time
print_time( HourOnly ) :-
   Minsonly is floor( HourOnly * 60 ),
   Hours is Minsonly // 60,
   Mins is Minsonly mod 60,
   print_digits_of_time( Hours ),
   print( ':' ),
   print_digits_of_time( Mins ).

write_flight_line( [] ).

write_flight_line( [flight(Depart,Arrive,DTimeHM)|List]) :-
   airport( Depart, Depart_name, _, _ ),
   airport( Arrive, Arrive_name, _, _),
   convert_hour(DTimeHM, DepartTime),
   arrival_time(flight(Depart,Arrive,DTimeHM), ArrivalTime),
   write('depart  '), write( Depart ),
      write('  '), write( Depart_name ),
      write('  '), print_time( DepartTime),
   nl,
   write('arrive  '), write( Arrive ),
      write('  '), write( Arrive_name ),
      write('  '), print_time( ArrivalTime),
   nl,
   write_flight_line( List ).

transfer_time(H1, T2) :-
   convert_hour(T2, H2),
   convert_minute(M1, H1),
   convert_minute(M2, H2),
   M1 + 29 < M2.

incoming_flight_arrival(flight(Dep,Arriv,DepTime)) :-
   arrival_time(flight(Dep,Arriv,DepTime), ArrivTime),
   ArrivTime < 24.

list_flight_line( Node, End, [flight(Node, Next, NDep)|Outlist] ) :-
   not(Node = End),
   flight(Node, Next, NDep),
   list_flight_line( Next, End, [flight(Node, Next, NDep)], Outlist ).

list_flight_line( Node, Node, _, [] ).
list_flight_line( Node, End,
   [flight(PDep,PArr,PDepTime)|Tried],
   [flight(Node, Next, NDep)|List] ) :-
   flight(Node, Next, NDep),

   arrival_time(flight(PDep,PArr,PDepTime), PArriv),
   transfer_time(PArriv, NDep),
   incoming_flight_arrival(flight(Node,Next,NDep)),
   Tried2 = append([flight(PDep,PArr,PDepTime)], Tried),
   not( member( Next, Tried2 )),
   not(Next = PArr),

   list_flight_line( Next, End,
   [flight(Node, Next, NDep)|Tried2],
      List ).

flight_time([flight(Dep, Arr, DTimeHM)|List], Length) :-
   length(List, 0),
   convert_hour(DTimeHM,DTimeH),
   arrival_time(flight(Dep, Arr, DTimeHM), ArrivalTime),
   Length is ArrivalTime - DTimeH.

flight_time([flight(Dep, Arr, DTimeHM)|List], Length) :-
   length(List, L),
   L > 0,
   flight_time(flight(Dep, Arr, DTimeHM), List, Length).


flight_time(flight(_, _, DTimeHM), [Head|List], Length) :-
   length(List, 0),
   convert_hour(DTimeHM, DTimeH),
   arrival_time(Head, ArrivalTime),
   Length is ArrivalTime - DTimeH.

flight_time(flight(Dep, Arr, DTimeHM), [_|List], Length) :-
   length(List, L),
   L > 0,
   flight_time(flight(Dep, Arr, DTimeHM), List, Length).


shortest_line(Depart, Arrive, List) :-
   list_flight_line(Depart, Arrive, List),
   cmp_short(Depart, Arrive, List).

cmp_short(Depart, Arrive, List) :-
   list_flight_line(Depart, Arrive, List2),
   flight_time(List, Length1),
   flight_time(List2, Length2),
   Length1 > Length2,
   !, fail.

cmp_short(_, _, _).


fly( Depart, Arrive ) :-
   shortest_line(Depart, Arrive, List),
   nl,
   write_flight_line(List),!.

fly( Depart, Depart ) :-
   % error messages for zero-fly queries.
   write('Error: It is a zero-fly'),
   !, fail.

fly( Depart, _ ) :-
   \+ airport(Depart, _, _, _),
   % error messages for nonexistent airports
   write('Departure airport does not exist.'),
   !, fail.

fly( _, Arrive ) :-
   \+ airport(Arrive, _, _, _),
   % error messages for nonexistent airports
   write('Arrival airport does not exist.'),
   !, fail.

fly( Depart, Arrive ) :-
   \+shortest_line(Depart, Arrive, _),
   write('Error: Do not get a proper itinerary.'),
   !, fail.


