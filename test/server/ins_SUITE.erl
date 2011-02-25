%%%-------------------------------------------------------------------
%%% File    : ins_SUITE.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : insert test suite
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
-module(ins_SUITE).
-include_lib("eunit/include/eunit.hrl").
%% Note: This directive should only be used in test suites.
%%-compile(export_all).
-import(protocol,[ot/2]).

%%----------------------
%% protocol ins test
%%----------------------
%% ins with nop test
%%----------------------
ins_nop_test() ->
    {ok,{nop,{ins,{1,2}}}} = ot({ins,{1,2}}, nop).
%%----------------------
%% ins with ins test
%%----------------------
%% Different parents different nodes
ins_ins1_test() ->
    {ok,{ins,{1,2}},{ins,{3,4}}} = ot({ins,{3,4}},{ins,{1,2}}).
%% Same parent different nodes
ins_ins2_test() ->
    {ok,{ins,{1,2}},{ins,{3,2}}} = ot({ins,{3,2}},{ins,{1,2}}).
%% Different parent same node
ins_ins3_test() ->
    {ok,{ins,{1,2}},{ins,{1,3}}} = ot({ins,{1,3}},{ins,{1,2}}).
%% Same parent same node
ins_ins4_test() ->
    {ok,{nop,nop}} = ot({ins,{1,2}},{ins,{1,2}}).
%%----------------------
%% ins with del test
%%----------------------
%% Different parents different nodes
ins_del1_test() ->
    {ok,{{del,{3,4}},{ins,{1,2}}}} = ot({ins,{1,2}}, {del,{3,4}}).
%% Different parents same node
ins_del2_test() ->
    {ok,{{del,{2,4}},{ins,{1,2}}}} = ot({ins,{1,2}}, {del,{2,4}}).
%% Same parent different nodes
ins_del3_test() ->
    {ok,{{del,{3,2}},{ins,{1,2}}}} = ot({ins,{1,2}}, {del,{3,2}}).
%% Same parent same node
ins_del4_test() ->
    {ok,{nop, {ins,{1,2}}}} = ot({ins,{1,2}}, {del,{1,2}}).
%%----------------------
%% ins with rel test
%%----------------------
% All Nodes are different
ins_rel1_test() ->
    {ok,{{rel,{2,3}},{ins,{1,4}}}} = ot({ins,{1,4}}, {rel,{2,3}}).
% First node is the same
ins_rel2_test() ->
    {ok,{{rel,{1,3}},{ins,{1,4}}}} = ot({ins,{1,4}}, {rel,{1,3}}).
% Second node is the same
ins_rel3_test() ->
    {ok,{{rel,{2,1}},{ins,{1,4}}}} = ot({ins,{1,4}}, {rel,{2,1}}).
% All nodes are the same
ins_rel4_test() ->
    {warn,{nop,{ins,{1,4}}}} = ot({ins,{1,4}}, {rel,{1,1}}).
% Both relation nodes are the same but the insert one is different
ins_rel5_test() ->
    {warn,{nop,{ins,{2,4}}}} = ot({ins,{2,4}}, {rel,{1,1}}).
%%----------------------
%% ins with brk test
%%----------------------
% All Nodes are different
ins_brk1_test() ->
    {ok,{{brk,{2,3}},{ins,{1,4}}}} = ot({ins,{1,4}}, {brk,{2,3}}).
% First node is the same
ins_brk2_test() ->
    {ok,{{brk,{1,3}},{ins,{1,4}}}} = ot({ins,{1,4}}, {brk,{1,3}}).
% Second node is the same
ins_brk3_test() ->
    {ok,{{brk,{2,1}},{ins,{1,4}}}} = ot({ins,{1,4}}, {brk,{2,1}}).
% All nodes are the same
ins_brk4_test() ->
    {warn,{nop,{ins,{1,4}}}} = ot({ins,{1,4}}, {brk,{1,1}}).
% Both break nodes are the same but the insert one is different
ins_brk5_test() ->
    {warn,{nop,{ins,{2,4}}}} = ot({ins,{2,4}}, {brk,{1,1}}).
