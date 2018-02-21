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
% prepReser(+Number, +Reservations, +Rooms, +Schedule,
%                     -TotalPaid, -Vals).
prepReser(_,[],-,_,0,[]).
prepReser(Number,[Res1|ReservaRest],Rooms,SchIn,Paid,Vals):-
  updateRoomBasedOnRes(Number,Res1,Rooms,SchIn,PayPerRoom),
  Num1 = Number +1,
  prepReser(Num1,ReservaRest,Rooms,SchIn,PaidRest,ValsSub).

% updateRoomBasedOnRes(+Number,+Res1,+Rooms,+SchIn,-PayPerRoom)
updateRoomBasedOnRes(Number,Res1,[Room|RoomsRest],SchIn,PayPerRoom):-


% printSchedule(Schedule).
%
% Prints schedule on the console.
%
% Schedule: Schedule to be printed.

printSchedule([]).

printSchedule([Sch|Schedule]) :-
    print(Sch),nl,
    printSchedule(Schedule).
