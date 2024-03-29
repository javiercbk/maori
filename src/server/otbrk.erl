%%%-------------------------------------------------------------------
%%% File    : otbrk.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : break command ot functions
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <javierlecuona@gmail.com>
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
-module(otbrk).
-import(otgeneral,[otrelation/2, otresolve/2]).
-import(response,[ok_response/2, warning_response/2]).
-export([otbrk/2]).

otbrk({brk, {Node, Node}}, nop) ->
    warning_response(nop, nop);
otbrk({brk, {Node, OtherNode}}, nop) ->
    ok_response(nop, {brk, {Node, OtherNode}});

%%-----------------
%%break & break
%%-----------------
otbrk({brk,Nodes},{brk,Nodes1})->
    otrelation({brk,Nodes},{brk,Nodes1});


%%-----------------
%%break & insert
%%-----------------
otbrk({brk,{Node,Node}},{ins,{Node,Parent}})->
    warning_response({ins,{Node,Parent}},nop);
otbrk({brk,{Node1,Node1}},{ins,{Node,Parent}}) ->
    warning_response({ins,{Node,Parent}},nop);
otbrk({brk,{Node,Node1}},{ins,{Node,Parent}}) ->
    ok_response({ins,{Node,Parent}},{brk,{Node,Node1}});
otbrk({brk,{Node1,Node}},{ins,{Node,Parent}}) ->
    ok_response({ins,{Node,Parent}},{brk,{Node1,Node}});
otbrk({brk,{Node1,Node2}},{ins,{Node,Parent}}) ->
    ok_response({ins,{Node,Parent}},{brk,{Node1,Node2}});


%%-----------------
%%relate & break
%%-----------------
%%resolve simple conflicts
%%matches {rel,{1,1}},{rel,{1,1}}
otbrk({brk,{Node,Node}},{rel,{Node1,Node1}}) ->
    otresolve({brk,{Node,Node}},{rel,{Node1,Node1}});
%%relate Node1 to Node twice so the are already sychronized
%%better to force break twice than losing data
otbrk({brk,{Node1,Node}},{rel,{Node1,Node}}) when Node =/= Node1 ->
    ok_response({rel,{Node1,Node}},nop);
%%relate Node1 to Node twice so the are already sychronized
%%better to force break twice than losing data
otbrk({brk,{Node1,Node}},{rel,{Node,Node1}})->
    ok_response({brk,{Node1,Node}},nop);
%%cant create/break relation between node and itself so nop is performed
otbrk({brk,{Node1,Node2}},{rel,{Node,Node}}) ->
    warning_response(nop,{brk,{Node1,Node2}});
%%relate Node to Itself and break Node1 with Node2 
%%cant create/break relation between node and itself so nop is performed
otbrk({brk,{Node,Node}},{rel,{Node1,Node2}}) ->
    warning_response({rel,{Node1,Node2}},nop);
otbrk({brk,{Node,Node1}},{rel,{Node2,Node}})->
    ok_response({rel,{Node2,Node}},{brk,{Node,Node1}});
otbrk({brk,{Node,Node1}},{rel,{Node,Node2}})->
    ok_response({rel,{Node,Node2}},{brk,{Node,Node1}});
otbrk({brk,{Node1,Node}},{rel,{Node,Node2}})->
    ok_response({rel,{Node,Node2}},{brk,{Node1,Node}});
otbrk({brk,{Node1,Node}},{rel,{Node2,Node}})->
    ok_response({rel,{Node2,Node}},{brk,{Node1,Node}});
%%relate Node1 with Node2 and break Node3 with Node4
otbrk({brk,{Node1,Node2}},{rel,{Node3,Node4}}) ->
    ok_response({rel,{Node3,Node4}},{brk,{Node1,Node2}});
%%resolve simple conflicts
otbrk({brk,Nodes},{rel,Nodes1}) ->
    otresolve({brk,Nodes},{rel,Nodes1});

%%-----------------
%%break & delete
%%-----------------
otbrk({brk,{Node,Node}},{del,{Node,Parent}})->
    warning_response({del,{Node,Parent}},nop);
otbrk({brk,{Node1,Node1}},{del,{Node,Parent}}) ->
    warning_response({del,{Node,Parent}},nop);
otbrk({brk,{Node,Node1}},{del,{Node,Parent}}) ->
    ok_response({del,{Node,Parent}},{brk,{Node,Node1}});
otbrk({brk,{Node1,Node}},{del,{Node,Parent}}) ->
    ok_response({del,{Node,Parent}},{brk,{Node1,Node}});
otbrk({brk,{Node1,Node2}},{del,{Node,Parent}}) ->
    ok_response({del,{Node,Parent}},{brk,{Node1,Node2}}).
