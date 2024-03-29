%%%-------------------------------------------------------------------
%%% File    : otrel.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : Relate operation transformation
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
-module(otrel).
-import(otgeneral,[otrelation/2, otresolve/2]).
-import(response,[ok_response/2, warning_response/2]).
-export([otrel/2]).


otrel({rel, {Node, Node}}, nop) ->
    warning_response(nop, nop);
otrel({rel, {Node, OtherNode}}, nop) ->
    ok_response(nop, {rel, {Node, OtherNode}});

%%-----------------
%%relate & relate
%%-----------------
otrel({rel,Nodes},{rel,Nodes1})->
    otrelation({rel,Nodes},{rel,Nodes1});

%%-----------------
%%relate & insert
%%-----------------
otrel({rel,{Node,Node}},{ins,{Node,Parent}})->
    warning_response({ins,{Node,Parent}},nop);
otrel({rel,{Node1,Node1}},{ins,{Node,Parent}}) ->
    warning_response({ins,{Node,Parent}},nop);
otrel({rel,{Node,Node1}},{ins,{Node,Parent}}) ->
    ok_response({ins,{Node,Parent}},{rel,{Node,Node1}});
otrel({rel,{Node1,Node}},{ins,{Node,Parent}}) ->
    ok_response({ins,{Node,Parent}},{rel,{Node1,Node}});
otrel({rel,{Node1,Node2}},{ins,{Node,Parent}}) ->
    ok_response({ins,{Node,Parent}},{rel,{Node1,Node2}});

%%-----------------
%%relate & break
%%-----------------
%%resolve simple conflicts
%%matches {rel,{1,1}},{brk,{1,1}}
otrel({rel,{Node,Node}},{brk,{Node1,Node1}}) ->
    otresolve({rel,{Node,Node}},{brk,{Node1,Node1}});
%%relate Node1 to Node twice so the are already sychronized
%%better to force break twice than losing data
otrel({rel,{Node1,Node}},{brk,{Node1,Node}}) when Node =/= Node1 ->
    ok_response(nop,{rel,{Node1,Node}});
%%relate Node1 to Node twice so the are already sychronized
%%better to force break twice than losing data
otrel({rel,{Node1,Node}},{brk,{Node,Node1}})->
    ok_response(nop,{rel,{Node1,Node}});
%%relate Node1 with Node2 and break Node to Itself
%%cant create/break relation between node and itself so nop is performed
otrel({rel,{Node1,Node2}},{brk,{Node,Node}}) ->
    warning_response(nop,{rel,{Node1,Node2}});
%%relate Node to Itself and break Node1 with Node2 
%%cant create/break relation between node and itself so nop is performed
otrel({rel,{Node,Node}},{brk,{Node1,Node2}}) ->
    warning_response({brk,{Node1,Node2}},nop);
otrel({rel,{Node,Node1}},{brk,{Node2,Node}})->
    ok_response({brk,{Node2,Node}},{rel,{Node,Node1}});
otrel({rel,{Node,Node1}},{brk,{Node,Node2}})->
    ok_response({brk,{Node,Node2}},{rel,{Node,Node1}});
otrel({rel,{Node1,Node}},{brk,{Node,Node2}})->
    ok_response({brk,{Node,Node2}},{rel,{Node1,Node}});
otrel({rel,{Node1,Node}},{brk,{Node2,Node}})->
    ok_response({brk,{Node2,Node}},{rel,{Node1,Node}});
%%relate Node1 with Node2 and break Node3 with Node4
otrel({rel,{Node1,Node2}},{brk,{Node3,Node4}}) ->
    ok_response({brk,{Node3,Node4}},{rel,{Node1,Node2}});
%%resolve simple conflicts
otrel({rel,Nodes},{brk,Nodes1}) ->
    otresolve({rel,Nodes},{brk,Nodes1});

%%-----------------
%%relate & delete
%%-----------------
otrel({rel,{Node,Node}},{del,{Node,Parent}})->
    warning_response({del,{Node,Parent}},nop);
otrel({rel,{Node1,Node1}},{del,{Node,Parent}}) ->
    warning_response({del,{Node,Parent}},nop);
otrel({rel,{Node,Node1}},{del,{Node,Parent}}) ->
    ok_response({del,{Node,Parent}},{rel,{Node,Node1}});
otrel({rel,{Node1,Node}},{del,{Node,Parent}}) ->
    ok_response({del,{Node,Parent}},{rel,{Node1,Node}});
otrel({rel,{Node1,Node2}},{del,{Node,Parent}}) ->
    ok_response({del,{Node,Parent}},{rel,{Node1,Node2}}).
