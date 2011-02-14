%%%-------------------------------------------------------------------
%%% File    : otnop.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : 
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <chuby@bones>
%%%-------------------------------------------------------------------
-module(otnop).

%% easy cases. Simple sync
otnop(nop, nop) ->
    ok_response(nop, nop);
otnop(nop, {ins, {Node, Parent}}) ->
    ok_response({ins, {Node, Parent}}, nop);
otnop(nop, {rel, {Node, OtherNode}}) ->
    ok_response({rel, {Node, OtherNode}}, nop);
otnop(nop, {rel, {Node, Node}}) ->
    warning_response(nop, nop);
otnop(nop, {brk, {Node, OtherNode}}) ->
    ok_response({brk, {Node, OtherNode}}, nop);
otnop(nop, {brk, {Node, Node}}) ->
    warning_response(nop, nop);
otnop(nop, {del, {Node, Parent}}) ->
    ok_response({del, {Node, Parent}}, nop);
