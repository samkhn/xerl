% Demo of high order functions mapped onto lists and used as comparators in sort
%
% 1> c(hif).
% 2> hif:convert_list_to_celsius([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
%

-module(hif).

-export([convert_list_to_celsius/1]).

convert_to_celsius({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_celsius({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_celsius(List) ->
    NewList = lists:map(fun convert_to_celsius/1, List),
    % In the HIF passed to sort, smaller elements will be moved to beginning of list
    lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) -> Temp1 < Temp2 end, NewList).
