%%%-------------------------------------------------------------------
%%% File    : otdel.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : 
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <chuby@bones>
%%%-------------------------------------------------------------------
-module(otdel).

otdel({del, {Node, Parent}}, nop) ->
    ok_response(nop, {del, {Node, Parent}});
