%%%-------------------------------------------------------------------
%%% File    : otnop.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : no operation ot functions
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <javierlecuona@gmail.com>
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
