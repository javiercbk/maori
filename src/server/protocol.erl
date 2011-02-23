%%%-------------------------------------------------------------------
%%% File    : protocol.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : Operational transformation protocol
%%%
%%% Created : 12 Feb 2011 by Javier Lecuona <javierlecuona@gmail.com>
%%%
%%% Copyright (C) 2011  Javier Lecuona
%%%
%%% This file is part of Maori.
%%%
%%% Maori is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% Maori is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>
%%%-------------------------------------------------------------------
-module(protocol).
-import(otnop,[otnop/2]).
-import(otins,[otins/2]).
-import(otrel,[otrel/2]).
-import(otbrk,[otbrk/2]).
-import(otdel,[otdel/2]).
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

