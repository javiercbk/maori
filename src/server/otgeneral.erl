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

%%relate/break Node to Node and relate/break Node1 to Node1
%%cant create/break relation between node and itself so nop is performed
%%matches {_,{1,1}},{_,{1,1}}
%%matches {_,{1,1}},{_,{2,2}}
otrelation({_,{Node,Node}},{_,{Node1,Node1}}) ->
    warning_response(nop,nop);
%%relate/break Node to Node and relate/break Node1 to Node
%%cant create/break relation between node and itself so nop is performed
%%matches {_,{1,1}},{_,{2,1}};
%%matches {_,{1,1}},{_,{1,3}};
%%matches {_,{1,1}},{_,{2,3}};
otrelation({_,{Node,Node}},{Command1,{Node1,Node2}}) ->
    warning_response({Command1,{Node1,Node2}},nop);
%%relate/break Node to Node and relate/break Node1 to Node
%%cant create/break relation between node and itself so nop is performed
%%matches {_,{2,1}},{_,{1,1}};
%%matches {_,{1,3}},{_,{1,1}};
%%matches {_,{2,3}},{_,{1,1}};
otrelation({Command1,{Node1,Node2}},{_,{Node,Node}}) ->
    warning_response(nop,{Command1,{Node1,Node2}});
%%relate/break Node to Node1 twice
%%matches {r/b,{1,2},{r/b,{1,2}}}
otrelation({Command,{Node,Node1}},{Command,{Node,Node1}}) ->
    ok_response(nop,nop);
%%relate/break Node1 to Node twice so the are already sychronized
%%matches {{r/b,{1,2},{r/b,{2,1}}}
otrelation({Command,{Node,Node1}},{Command,{Node1,Node}}) ->
    ok_response(nop,nop);
%%relate/break Node to Node1
%%matches {{r/b,{1,2},{b/r,{1,2}}}
%%matches {{r/b,{2,1},{b/r,{1,2}}}
%%matches {{r/b,{1,2},{b/r,{3,2}}}
%%matches {{r/b,{1,2},{b/r,{2,3}}}
%%matches {{r/b,{2,3},{b/r,{1,2}}}
%%matches {{r/b,{3,2},{b/r,{1,2}}}
%%matches {{r/b,{2,3},{b/r,{2,1}}}
%%matches {{r/b,{3,2},{b/r,{2,1}}}
%%matches {{r/b,{3,2},{b/r,{1,2}}}
%%matches {{r/b,{1,2},{b/r,{3,4}}}
otrelation({Command,{Node1,Node2}},{Command1,{Node3,Node4}})->
    ok_response({Command1,{Node3,Node4}},{Command,{Node1,Node2}}).

%%relate Node to Node and break Node to Node
%%cant create/break relation between node and itself so nop is performed
%%matches {{_,{1,1},{_,{1,1}}}
otresolve({_,{Node,Node}},{_,{Node,Node}}) ->
    warning_response(nop,nop);
%%relate Node to Node and break Node1 to Node1
%%cant create/break relation between node and itself so nop is performed
%%matches {{_,{1,1},{_,{2,2}}}
otresolve({_,{Node,Node}},{_,{Node1,Node1}}) ->
    warning_response(nop,nop);
%%relate Node to Node and break Node1 to Node
%%cant create/break relation between node and itself so nop is performed
%%matches {{_,{1,1},{_,{2,1}}}
otresolve({_,{Node,Node}},{Command2,{Node1,Node}}) ->
    warning_response({Command2,{Node1,Node}},nop);
%%relate Node1 to Node and break Node to Node
%%cant create/break relation between node and itself so nop is performed
%%matches {{_,{2,1},{_,{1,1}}}
otresolve({Command1,{Node1,Node}},{_,{Node,Node}}) ->
    warning_response(nop,{Command1,{Node1,Node}});
%%relate Node to Node and break Node to Node1
%%cant create/break relation between node and itself so nop is performed
%%matches {{_,{1,1},{_,{1,2}}}
otresolve({_,{Node,Node}},{Command2,{Node,Node1}}) ->
    warning_response({Command2,{Node,Node1}},nop);
%%relate Node to Node1 and break Node to Node
%%cant break relation between node and itself so nop is performed
%%matches {{_,{1,2},{_,{1,1}}}
otresolve({Command1,{Node,Node1}},{_,{Node,Node}}) ->
    warning_response(nop,{Command1,{Node,Node1}}).
