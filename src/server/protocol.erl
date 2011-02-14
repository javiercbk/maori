%%%-------------------------------------------------------------------
%%% File    : protocol.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : Operational transformation protocol
%%%
%%% Created : 12 Feb 2011 by Javier Lecuona <chuby@bones>
%%%-------------------------------------------------------------------
-module(protocol).
%%sirve este import???
-import(otnop).
-import(otins).
-import(otrel).
-import(otbrk).
-import(otdel).
-export([ot/2]).

% function: ot*(ClientAction, ServerAction) -> {ok,{ClientPerform, ServerPerform}} 
%% OPERATION MAPPING
%% nop = no operation pending
%% ins = insert Node
%% rel = create relation between Node and OtherNode
%% brk = break relation between Node and Other Node
%% del = delete Node From Parent

% function ot(Operation, OtherOperation) -> {ok,{ClientPerform, ServerPerform}} | 
% {warn,{ClientPerform, ServerPerform}} | {warn,{ClientPerform, Reason}, {ServerPerform, Reason}}
% acts as a dispatcher for the operation transformation
ot(nop,OtherOperation)->
    otnop(nop,OtherOperation);
ot({ins,Commands},OtherOperation)->
    otins({ins,Commands},OtherOperation);
ot({rel,Commands},OtherOperation)->
    otrel({rel,Commands},OtherOperation);
ot({brk,Commands},OtherOperation)->
    otbrk({brk,Commands},OtherOperation);
ot({del,Commands},OtherOperation)->
    otdel({del,Commands},OtherOperation).

