:- use_module(library(clpfd)).
:- use_module(library(lists)).

% hotel(Rooms, RoomPricePerDay, Reservations, Income).
%
% Main predicate of the program.
%
% +Rooms: List of numbers. Those numbers represent size of rooms.
% +RoomPricePerDay: List of touples of two.
% -> example :prices for 14 days of three and four people sized rooms
%             [[[3],[5,5,5,5,6,7,7,5,5,5,5,6,9,8]],
%             [[4],[6,6,6,7,7,8,8,8,4,4,4,6,11,10]]]
% +Reservations: List of all reservations. List consist of sublists :
%   -> [[[potentialStart,potentialEnd],
%       #days,
%       #people,
%       maximalPriceToPay],....]
% -Income: Total income of the hotel.

hotel(Rooms, [[[H],[PricesPerDay]]|T], Reservations, Income) :-
    length(PricesPerDay, CountDays),
    length(Reservations, CountReservations),
    prepareSchedule(CountDays, CountReservations, Rooms, Schedule),
    % teoreticky rozsirit o cleaning, ked tam niekto byva alebo den pred tym ako tam niekto nastupi
    prepReser(1, Reservations, Rooms, Schedule,[[[H],[PricesPerDay]]|T],UsageFee,Vals),
%    Income #= Paid - Cleaning - CancelFee * Cancelled,
%    append(Schedule, SchVals),
%    append(Vals, SchVals, FinalVals),
%    Var #=  Income - WasteFee * Wasted,
%    labeling([maximize(Var)], FinalVals),
%    printSchedule(Schedule).



% prepareSchedule(Days, Res, Rooms, Schedule, Sum).
%
% Prepares variables for each room and day.
%
% Days: Total number of days.
% Res: Total number of reservations.
% Rooms: List of all rooms.
% Schedule: Table of variables indicating which reservation uses the room.
% Sum: Sum of all variables in Schedule.

prepareSchedule(_, _, [], []).

prepareSchedule(Days, Res, [_|Rooms], [Sch|Schedule]) :-
    prepareRoomSchedule(Days,Res, Sch),
    prepareSchedule(Days, Res, Rooms, Schedule).
%    Sum #= RoomSum + TmpSum.



% prepareRoomSchedule(Days, Res, Schedule, Sum).
%
% Prepares variables for one room and each day.
%
% Days: Number of remaining days.
% Res: Total number of reservations.
% Schedule: List of variables indicating which reservation uses the room.
% Sum: Sum of all variables in Schedule.

prepareRoomSchedule(0, _, []).

prepareRoomSchedule(Days, Res, [Sch|Schedule]) :-
    Days > 0, !,
    TmpDays is Days - 1,
    Sch in 0..Res,
    prepareRoomSchedule(TmpDays, Res, Schedule).
%    Sum #= Sch + TmpSum.


% prepReser // prepareReservations
% predicate goes over all reservations and call updateRoomBasedOnRes,
% which updates every room based on reservation preperties

% prepReser(+Number, +Reservations, +Rooms, +Schedule,
%                     -TotalPaid, -Vals).
prepReser(_,[],-,_,0,[]).
prepReser(Number,[Res1|ReservaRest],Rooms,SchIn,Paid,Vals):-
  updateRoomBasedOnRes(Number,Res1,Rooms,SchIn,PayPerRoom),
  Num1 = Number +1,
  prepReser(Num1,ReservaRest,Rooms,SchIn,PaidRest,ValsSub).

% updateRoomBasedOnRes(+Number,+Res1,+Rooms,+SchIn,-PayPerRoom)
updateRoomBasedOnRes(Number,([Start,End|_],Days,People,MaxPrice),[Room|RoomsRest],[Sch|SchRest],PayPerRoom):-
updateSchedule(Number,Start,End,Room,People,Sch), % take one room and its schedule and bind it
updateRoomBasedOnRes(Number,([Start,End|_],Days,People,MaxPrice),RoomsRest,SchRest,PayPerRoom).

% getRightDayInRoom predicate iterates over days till it is equal of first possible day of reservation
% getRightDayInRoom(+Number,+Start,+Room,+People,+Sch)
updateSchedule(Number,Start,End,Room,People,Sch):-
  Room#>=People,!, % this room is suitable for the people from reservation
  Day=1,
updateBasedOnDay(Day,Number,Start,End,Sch).

  updateSchedule(Number,Start,End,Room,People,Sch):-
    Room#<People,!, % this room is NOT suitable for the people from reservation
updateBasedOnDayRemoveAll(Number,Sch). % This could be also done with updateBasedOnDay predicate with Start and End parameter being f.e. 2 and 1 respectively

% predicate updates schedules
% when the day is not the day that reservation lasts (between its possible start and end)
% then this reservation's number is removed from schedule of this day
% updateBasedOnDay(+Day,+Number,+Start,+End,+Sch)
updateBasedOnDay(_,_,_,_,[]).
updateBasedOnDay(Day,Number,Start,End,[Sch|SchRest]):-
  Day >=Start,
  Day =< End,!, % we are in possible day for reservation
  DayNext is Day + 1; % so we do nothing and move on to the next day
  updateBasedOnDay(DayNext,Number,Start,End,SchRest).

% here we remove reservation's number from schedule where it does not belong
updateBasedOnDay(Day,Number,Start,End,[Sch|SchRest]):-
  % this could be included but it is not necessary because of previous predicate with cut.
  % (Day <Start;Day > End),
  Sch#\=Number, % on this day reservation is not posssible because the reservation is not happening during this day.
  DayNext is Day + 1;
  updateBasedOnDay(DayNext,Number,Start,End,SchRest).

% Room is not suitable for reservation and we are romoving it.
% Reservation is represented by number and we are removing this number.
% updateBasedOnDayRemoveAll(+Number,+Sch).
updateBasedOnDayRemoveAll(_,[]).
updateBasedOnDayRemoveAll(Number,[Sch|SchRest]):-
Sch#\=Number,
updateBasedOnDayRemoveAll(Number,SchRest).

% printSchedule(Schedule).
%
% Prints schedule on the console.
%
% Schedule: Schedule to be printed.

printSchedule([]).

printSchedule([Sch|Schedule]) :-
    print(Sch),nl,
    printSchedule(Schedule).
