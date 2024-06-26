% Demonstration of BIF trunc, if guard, case match
% trunc is used to cutoff the decimals from floats
%
% 1> c(month_length).
% 2> month_length:days(2004, feb).
% 3> month_length:days(1947, aug).
%

-module(month_length).

-export([days/2]).

days(Year, Month) ->
    %% All years divisible by 400 are leap years
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above).
    Leap = if
               trunc(Year / 400) * 400 == Year ->
                   leap;
               trunc(Year / 100) * 100 == Year ->
                   not_leap;
               trunc(Year / 4) * 4 == Year ->
                   leap;
               true ->
                   not_leap
           end,
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.
