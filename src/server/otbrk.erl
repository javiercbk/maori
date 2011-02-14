%%%-------------------------------------------------------------------
%%% File    : otbrk.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : 
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <chuby@bones>
%%%-------------------------------------------------------------------
-module(otbrk).
-import(otgeneral).
-export([otbrk/2]).

otbrk({brk, {Node, OtherNode}}, nop) ->
    ok_response(nop, {brk, {Node, OtherNode}});
otbrk({brk, {Node, Node}}, nop) ->
    warning_response({nop,'Cannot break Node from itself'}, nop);


%%-----------------
%%break & insert
%%-----------------
otbrk({brk,{Node,Node}},{ins,(Node,Parent)})->
    warning_response({ins,(Node,Parent)},nop);
otbrk({brk,{Node1,Node1}},{ins,(Node,Parent)}) ->
    warning_response({ins,(Node,Parent)},nop);
otbrk({brk,{Node,Node1}},{ins,(Node,Parent)}) ->
    ok_response({ins,(Node,Parent)},{brk,{Node,Node1}});
otbrk({brk,{Node1,Node}},{ins,(Node,Parent)}) ->
    ok_response({ins,(Node,Parent)},{brk,{Node1,Node}});
otbrk({brk,{Node1,Node2}},{ins,(Node,Parent)}) ->
    ok_response({ins,(Node,Parent)},{brk,{Node1,Node2}});

%%-----------------
%%break & relate
%%-----------------
%%break Node to Node1 twice so the are already sychronized
%%better to force break twice than losing data
otbrk({brk,{Node,Node1}},{rel,{Node,Node1}}) ->
    ok_response(nop,{rel,{Node,Node1}});
%%break Node1 to Node twice so the are already sychronized
%%better to force break twice than losing data
otbrk({rel,{Node1,Node}},{brk,{Node1,Node}}) ->
    ok_response(nop,{rel,{Node1,Node}});
%%break Node1 to Node twice so the are already sychronized
%%better to force break twice than losing data
otbrk({rel,{Node1,Node}},{brk,{Node,Node1}})->
    ok_response(nop,{rel,{Node1,Node}});
%%break Node1 with Node2 and break Node to Itself
%%cant create/break relation between node and itself so nop is performed
otbrk({rel,{Node1,Node2}},{brk,{Node,Node}}) ->
    warning_response(nop,{rel,{Node1,Node2}});
%%break Node to Itself and break Node1 with Node2 
%%cant create/break relation between node and itself so nop is performed
otbrk({rel,{Node,Node}},{brk,{Node1,Node2}}) ->
    warning_response({brk,{Node1,Node2}},nop);
otbrk({rel,{Node,Node1}},{brk,{Node2,Node}})->
    ok_response({brk,{Node2,Node}},{rel,{Node,Node1}});
otbrk({rel,{Node,Node1}},{brk,{Node,Node2}})->
    ok_response({brk,{Node,Node2}},{rel,{Node,Node1}});
otbrk({rel,{Node1,Node}},{brk,{Node,Node2}})->
    ok_response({brk,{Node,Node2}},{rel,{Node1,Node}});
otbrk({rel,{Node1,Node}},{brk,{Node2,Node}})->
    ok_response({brk,{Node2,Node}},{rel,{Node1,Node}});
%%break Node1 with Node2 and break Node3 with Node4
otbrk({rel,{Node1,Node2}},{brk,{Node3,Node4}}) ->
    ok_response({brk,{Node3,Node4}},{rel,{Node1,Node2}});
%%resolve simple conflicts
otbrk({brk,Nodes},{rel,Nodes})->
    otresolve({brk,Nodes},{rel,Nodes});
otbrk({brk,Nodes},{brk,Nodes1}) ->
    otresolve({brk,Nodes},{rel,Nodes1});

%%-----------------
%%break & delete
%%-----------------
otbrk({rel,{Node,Node}},{del,(Node,Parent)})->
    warning_response({del,(Node,Parent)},nop);
otbrk({rel,{Node1,Node1}},{del,(Node,Parent)}) ->
    warning_response({del,(Node,Parent)},nop);
otbrk({rel,{Node,Node1}},{del,(Node,Parent)}) ->
    ok_response({del,(Node,Parent)},{rel,{Node,Node1}});
otbrk({rel,{Node1,Node}},{del,(Node,Parent)}) ->
    ok_response({del,(Node,Parent)},{rel,{Node1,Node}});
otbrk({rel,{Node1,Node2}},{del,(Node,Parent)}) ->
    ok_response({del,(Node,Parent)},{rel,{Node1,Node2}});
