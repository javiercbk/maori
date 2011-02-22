%%%-------------------------------------------------------------------
%%% File    : otgeneral.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : General functions of ot
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
-module(otgeneral).
-import(response,[ok_response/2,warning_response/2]).
-export([otrelation/2,otresolve/2]).

%%relate/break Node to Node and relate/break Node to Node
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node,Node}},{Command,{Node,Node}}) ->
    warning_response(nop,nop);
%%relate/break Node to Node and relate/break Node1 to Node1
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node,Node}},{Command,{Node1,Node1}}) ->
    warning_response(nop,nop);
%%relate/break Node to Node and relate/break Node1 to Node
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node,Node}},{Command,{Node1,Node}}) ->
    warning_response({Command,{Node1,Node}},nop);
%%relate/break Node1 to Node and relate/break Node to Node
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node1,Node}},{Command,{Node,Node}}) ->
    warning_response(nop,{Command,{Node1,Node}});
%%relate/break Node to Node and relate/break Node to Node1
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node,Node}},{Command,{Node,Node1}}) ->
    warning_response({Command,{Node,Node1}},nop);
%%relate/break Node to Node1 and relate/break Node to Node
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node,Node1}},{Command,{Node,Node}}) ->
    warning_response(nop,{Command,{Node,Node1}});
%%relate/break Node to Node1 twice so the are already sychronized
otrelation({Command,{Node,Node1}},{Command,{Node,Node1}}) ->
    ok_response(nop,nop);
%%relate/break Node1 to Node twice so the are already sychronized
otrelation({Command,{Node1,Node}},{Command,{Node1,Node}}) ->
    ok_response(nop,nop);
%%relate/break Node1 to Node twice so the are already sychronized
%%it's the same operation!!!
otrelation({Command,{Node1,Node}},{Command,{Node,Node1}})->
    ok_response(nop,nop);
otrelation({Command,{Node,Node}},{Command,{Node1,Node1}})->
    warning_response(nop,nop);
%%relate/break Node1 with Node2 and relate/break Node to Itself
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node1,Node2}},{Command,{Node,Node}}) ->
    warning_response(nop,{Command,{Node1,Node2}});
%%relate/break Node to Itself and relate/break Node1 with Node2 
%%cant create/break relation between node and itself so nop is performed
otrelation({Command,{Node,Node}},{Command,{Node1,Node2}}) ->
    warning_response({Command,{Node1,Node2}},nop);
otrelation({Command,{Node,Node1}},{Command,{Node2,Node}})->
    ok_response({Command,{Node2,Node}},{Command,{Node,Node1}});
otrelation({Command,{Node,Node1}},{Command,{Node,Node2}})->
    ok_response({Command,{Node,Node2}},{Command,{Node,Node1}});
otrelation({Command,{Node1,Node}},{Command,{Node,Node2}})->
    ok_response({Command,{Node,Node2}},{Command,{Node1,Node}});
otrelation({Command,{Node1,Node}},{Command,{Node2,Node}})->
    ok_response({Command,{Node2,Node}},{Command,{Node1,Node}});
%%relate/break Node1 with Node2 and relate/break Node3 with Node4
otrelation({Command,{Node1,Node2}},{Command,{Node3,Node4}}) ->
    ok_response({Command,{Node3,Node4}},{Command,{Node1,Node2}});
%%Relate/Break Node1 with OtherNode and nop
%%cant create/break relation between node and itself so nop is performed
otrelation({_, {Node, Node}}, nop) ->
    warning_response(nop, nop);
%%Relate/Break Node1 with OtherNode and nop
otrelation({Command, {Node, OtherNode}}, nop) ->
    ok_response(nop, {Command, {Node, OtherNode}}).
%%relate Node to Node and break Node to Node
%%cant create/break relation between node and itself so nop is performed
otresolve({_1,{Node,Node}},{_2,{Node,Node}}) ->
    warning_response(nop,nop);
%%relate Node to Node and break Node1 to Node1
%%cant create/break relation between node and itself so nop is performed
otresolve({_,{Node,Node}},{_,{Node1,Node1}}) ->
    warning_response(nop,nop);
%%relate Node to Node and break Node1 to Node
%%cant create/break relation between node and itself so nop is performed
otresolve({_,{Node,Node}},{Command2,{Node1,Node}}) ->
    warning_response({Command2,{Node1,Node}},nop);
%%relate Node1 to Node and break Node to Node
%%cant create/break relation between node and itself so nop is performed
otresolve({Command1,{Node1,Node}},{_,{Node,Node}}) ->
    warning_response(nop,{Command1,{Node1,Node}});
%%relate Node to Node and break Node to Node1
%%cant create/break relation between node and itself so nop is performed
otresolve({_,{Node,Node}},{Command2,{Node,Node1}}) ->
    warning_response({Command2,{Node,Node1}},nop);
%%relate Node to Node1 and break Node to Node
%%cant break relation between node and itself so nop is performed
otresolve({Command1,{Node,Node1}},{_,{Node,Node}}) ->
    warning_response(nop,{Command1,{Node,Node1}}).
