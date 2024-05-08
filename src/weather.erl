% Covers a few interesting topics
% 1. io format width
% 2. how to handle lists w/ recursion
% 3. if guards
% 4. tuples for grouping or annotating data
%    e.g. if you see convert(3, inch) does this mean 3 to inches or 3 inches?
%    better to group them as {inch, 3} or {celsius, 15}.
%
% Demo
% 1> c(weather).
% 2> weather:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
%

-module(weather).

-export([format_temps/1]).

format_temps(ListOfCities) ->
    ConvertedList = convert_list_to_celsius(ListOfCities),
    print_temperature(ConvertedList),
    {MaxCity, MinCity} = find_max_min(ConvertedList),
    print_max_min(MaxCity, MinCity).

convert_list_to_celsius([{Name, {f, Temperature}} | Rest]) ->
    ConvertedCity = {Name, {c, (Temperature - 32) * 5 / 9}},
    [ConvertedCity | convert_list_to_celsius(Rest)];
convert_list_to_celsius([City | Rest]) ->
    [City | convert_list_to_celsius(Rest)];
convert_list_to_celsius([]) ->
    [].

print_temperature([{Name, {c, Temperature}} | Rest]) ->
    io:format("~-15w ~w c~n", [Name, Temperature]),
    print_temperature(Rest);
print_temperature([]) ->
    ok.

find_max_min([City | Rest]) ->
    find_max_min(Rest, City, City).

find_max_min([{Name, {c, Temperature}} | Rest], {MaxName, {c, MaxTemperature}}, {MinName, {c, MinTemperature}}) ->
    if
        Temperature > MaxTemperature ->
            MaxCity = {Name, {c, Temperature}};
        true ->
            MaxCity = {MaxName, {c, MaxTemperature}}
    end,
    if
        Temperature < MinTemperature ->
            MinCity = {Name, {c, Temperature}};
        true ->
            MinCity = {MinName, {c, MinTemperature}}
    end,
    find_max_min(Rest, MaxCity, MinCity);
find_max_min([], MaxCity, MinCity) ->
    {MaxCity, MinCity}.

print_max_min({MaxName, {c, MaxTemperature}}, {MinName, {c, MinTemperature}}) ->
    io:format("Max temperature was ~w c in ~w~n", [MaxTemperature, MaxName]),
    io:format("Min temperature was ~w c in ~w~n", [MinTemperature, MinName]).
