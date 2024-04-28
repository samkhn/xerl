% Demonstrates how to work with maps
%
% To try it out,
% 1> c(color).
% 2> C1 = color:new(0.3, 0.4, 0.5, 1.0).
% 3> C2 = color:new(1.0, 0.8, 0.1, 0.3).
% 4> color:blend(C1, C2).
% 5> color:blend(C2, C1).
%
-module(color).

-export([new/4, blend/2]).

-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

% Constructs a pixel
new(R, G, B, A) when ?is_channel(R), ?is_channel(G), ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.

blend(Src, Dst) ->
    blend(Src, Dst, alpha(Src, Dst)).

blend(Src, Dst, Alpha) when Alpha > 0.0 ->
    Dst#{
         red := red(Src, Dst) / Alpha,
         green := green(Src, Dst) / Alpha,
         blue := blue(Src, Dst) / Alpha,
         alpha := Alpha
        };
blend(_, Dst, _) ->
    Dst#{
         red := 0.0,
         green := 0.0,
         blue := 0.0,
         alpha := 0.0
        }.

alpha(#{alpha := SrcAlpha}, #{alpha := DstAlpha}) ->
    SrcAlpha + DstAlpha * (1.0 - SrcAlpha).

red(#{red := SrcValue, alpha := SrcAlpha}, #{red := DstValue, alpha := DstAlpha}) ->
    SrcValue * SrcAlpha + DstValue * DstAlpha * (1.0 - SrcAlpha).
green(#{green := SrcValue, alpha := SrcAlpha}, #{green := DstValue, alpha := DstAlpha}) ->
    SrcValue * SrcAlpha + DstValue * DstAlpha * (1.0 - SrcAlpha).
blue(#{blue := SrcValue, alpha := SrcAlpha}, #{blue := DstValue, alpha := DstAlpha}) ->
    SrcValue * SrcAlpha + DstValue * DstAlpha * (1.0 - SrcAlpha).
