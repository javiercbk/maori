%%%-------------------------------------------------------------------
%%% File    : otins.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : 
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <chuby@bones>
%%%-------------------------------------------------------------------
-module(otins).
-import(response).
-export([otins/2]).

%%*****************
%%inserts
%%-----------------
otins({ins, {Node, Parent}}, nop) ->
    ok_response(nop, {ins, {Node, Parent}});
%%insert & insert
%%same node same parent
otins({ins,{Node,Parent}},{ins,{Node,Parent}}) ->
    ok_response(nop, nop);
%%different Parent but same nodes
otins({ins,{Node,Parent1}},{ins,{Node,Parent2}}) ->
    ok_response({ins,{Node,Parent2}},{ins,{Node,Parent1}});
%%different nodes same parent
otins({ins,{Node1,Parent}},{ins,{Node2,Parent}}) ->
    ok_response({ins,{Node,Parent}},{ins,{Node1,Parent}});
%%different nodes different parents
otins({ins,{Node1,Parent1}},{ins,{Node2,Parent2}})->
    ok_response({ins,{Node2,Parent2}},{ins,{Node1,Parent1}});

%%-----------------
%%insert & relate
%%insert Node in Parent and Relate other node With Other node
otins({ins,{Node,Parent}},{rel,{Node1,Node2}}) ->
    ok_response({rel,{Node1,Node2}}, {ins,{Node,Parent}});
%%insert Node in Parent and relate same node to other node
otins({ins,{Node,Parent}},{rel,{Node,Node2}}) ->
    ok_response({rel,{Node1,Node2}},{ins,{Node,Parent}});
otins({ins,{Node,Parent}},{rel,{Node2,Node}}) ->
    ok_response({rel,{Node2,Node}},{ins,{Node,Parent}});
%%insert Node in Parent and relate same node to same node
%%cant create create relation between node and itself so nop is performed
otins({ins,{Node,Parent}},{rel,{Node,Node}}) ->
    ok_response(nop, {ins,{Node,Parent}});

%%-----------------
%%insert & break
%%insert a Node into Parent and break Node1 from Node2
otins({ins,{Node,Parent}},{brk,{Node1,Node2}}) ->
    ok_response({brk,{Node1,Node2}}, {ins,{Node,Parent}});
%%insert Node into Parent and break Node from Node1
otins({ins,{Node,Parent}},{brk,{Node,Node1}}) ->
    ok_response({brk,{Node,Node1}}, {ins,{Node,Parent}});
%%insert Node into Parent and break Node from Node
%%cant break Node from Node so nop is performed
otins({ins,{Node,Parent}},{brk,{Node,Node}}) ->
    ok_response(nop, {ins,{Node,Parent}});

%%-----------------
%%insert & delete
%%insert Node into Parent and delete Node1 from Parent1
otins({ins,{Node,Parent}},{del,{Node1,Parent1}}) ->
    ok_response({del,{Node1,Parent1}}, {ins,{Node,Parent}});
%%insert Node into Parent and delete Node from Parent1
otins({ins,{Node,Parent}},{del,{Node,Parent1}}) ->
    ok_response({del,{Node,Parent1}}, {ins,{Node,Parent}});
%%insert Node into Parent and delete Node from Parent
%%is better to force to delete twice than lose data
otins({ins,{Node,Parent}},{del,{Node,Parent}}) ->
    ok_response(nop, {ins,{Node,Parent}}).
