%%%-------------------------------------------------------------------
%%% File    : otdel.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : delete ot functions
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
-module(otdel).
-import(response,[ok_response/2, warning_response/2]).
-export([otdel/2]).

otdel({del, {Node, Parent}}, nop) ->
    ok_response(nop, {del, {Node, Parent}});

%%delete & delete
%%same node same parent
otdel({del,{Node,Parent}},{del,{Node,Parent}}) ->
    ok_response(nop, nop);
%%different Parent but same nodes
otdel({del,{Node,Parent1}},{del,{Node,Parent2}}) ->
    ok_response({del,{Node,Parent2}},{del,{Node,Parent1}});
%%different nodes same parent
otdel({del,{Node1,Parent}},{del,{Node2,Parent}}) ->
    ok_response({del,{Node2,Parent}},{del,{Node1,Parent}});
%%different nodes different parents
otdel({del,{Node1,Parent1}},{del,{Node2,Parent2}})->
    ok_response({del,{Node2,Parent2}},{del,{Node1,Parent1}});

%%-----------------
%%delete & relate
%%delete Node in Parent and relate same node to other node
otdel({del,{Node,Parent}},{rel,{Node,Node2}}) ->
    ok_response({rel,{Node,Node2}},{del,{Node,Parent}});
%%
otdel({del,{Node,Parent}},{rel,{Node2,Node}}) ->
    ok_response({rel,{Node2,Node}},{del,{Node,Parent}});
%%delete Node in Parent and relate same node to same node
%%cant create create relation between node and itself so nop is performed
otdel({del,{Node,Parent}},{rel,{Node,Node}}) ->
    ok_response(nop, {del,{Node,Parent}});
%%delete Node in Parent and Relate other node With Other node
otdel({del,{Node,Parent}},{rel,{Node1,Node2}}) ->
    ok_response({rel,{Node1,Node2}}, {del,{Node,Parent}});


%%-----------------
%%delete & break
%%delete Node into Parent and break Node from Node1
otdel({ins,{Node,Parent}},{brk,{Node,Node1}}) ->
    ok_response({brk,{Node,Node1}}, {ins,{Node,Parent}});
%%delete Node into Parent and break Node from Node
%%cant break Node from Node so nop is performed
otdel({ins,{Node,Parent}},{brk,{Node,Node}}) ->
    ok_response(nop, {ins,{Node,Parent}});
%%delete a Node into Parent and break Node1 from Node2
otdel({ins,{Node,Parent}},{brk,{Node1,Node2}}) ->
    ok_response({brk,{Node1,Node2}}, {ins,{Node,Parent}});

%%-----------------
%%delete & insert
%%delete Node into Parent and delete Node from Parent1
otdel({ins,{Node,Parent}},{del,{Node,Parent1}}) ->
    ok_response({del,{Node,Parent1}}, {ins,{Node,Parent}});
%%delete Node into Parent and delete Node from Parent
%%is better to force to delete twice than lose data
otdel({ins,{Node,Parent}},{del,{Node,Parent}}) ->
    ok_response(nop, {ins,{Node,Parent}});
%%delete Node into Parent and delete Node1 from Parent1
otdel({ins,{Node,Parent}},{del,{Node1,Parent1}}) ->
    ok_response({del,{Node1,Parent1}}, {ins,{Node,Parent}}).
