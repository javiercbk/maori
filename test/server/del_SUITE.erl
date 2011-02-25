%%%-------------------------------------------------------------------
%%% File    : del_SUITE.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : delete test suite
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
-module(del_SUITE).
-include_lib("eunit/include/eunit.hrl").
%% Note: This directive should only be used in test suites.
%%-compile(export_all).
-import(protocol,[ot/2]).

%%----------------------
%% protocol del test
%%----------------------
%% del with nop test
%%----------------------
del_nop_test() ->
    {ok,{nop,{del,{1,2}}}} = ot({del,{1,2}}, nop).
%%----------------------
%% del with ins test
%%----------------------
%% Different parents different nodes
del_ins1_test() ->
    {ok,{ins,{1,2}},{del,{3,4}}} = ot({del,{3,4}},{ins,{1,2}}).
%% Same parent different nodes
del_ins2_test() ->
    {ok,{ins,{1,2}},{del,{3,2}}} = ot({del,{3,2}},{ins,{1,2}}).
%% Different parent same node
del_ins3_test() ->
    {ok,{ins,{1,2}},{del,{1,3}}} = ot({del,{1,3}},{ins,{1,2}}).
%% Same parent same node
del_ins4_test() ->
    {ok,{nop,nop}} = ot({del,{1,2}},{ins,{1,2}}).
%%----------------------
%% del with del test
%%----------------------
%% Different parents different nodes
del_del1_test() ->
    {ok,{{del,{3,4}},{del,{1,2}}}} = ot({del,{1,2}}, {del,{3,4}}).
%% Different parents same node
del_del2_test() ->
    {ok,{{del,{2,4}},{del,{1,2}}}} = ot({del,{1,2}}, {del,{2,4}}).
%% Same parent different nodes
del_del3_test() ->
    {ok,{{del,{3,2}},{del,{1,2}}}} = ot({del,{1,2}}, {del,{3,2}}).
%% Same parent same node
del_del4_test() ->
    {ok,{nop, nop}} = ot({del,{1,2}}, {del,{1,2}}).
%%----------------------
%% del with rel test
%%----------------------
% All Nodes are different
del_rel1_test() ->
    {ok,{{rel,{2,3}},{del,{1,4}}}} = ot({del,{1,4}}, {rel,{2,3}}).
% First node is the same
del_rel2_test() ->
    {ok,{{rel,{1,3}},{del,{1,4}}}} = ot({del,{1,4}}, {rel,{1,3}}).
% Second node is the same
del_rel3_test() ->
    {ok,{{rel,{2,1}},{del,{1,4}}}} = ot({del,{1,4}}, {rel,{2,1}}).
% All nodes are the same
del_rel4_test() ->
    {warn,{nop,{del,{1,4}}}} = ot({del,{1,4}}, {rel,{1,1}}).
% Both relation nodes are the same but the delete one is different
del_rel5_test() ->
    {warn,{nop,{del,{2,4}}}} = ot({del,{2,4}}, {rel,{1,1}}).
%%----------------------
%% del with brk test
%%----------------------
% All Nodes are different
del_brk1_test() ->
    {ok,{{brk,{2,3}},{del,{1,4}}}} = ot({del,{1,4}}, {brk,{2,3}}).
% First node is the same
del_brk2_test() ->
    {ok,{{brk,{1,3}},{del,{1,4}}}} = ot({del,{1,4}}, {brk,{1,3}}).
% Second node is the same
del_brk3_test() ->
    {ok,{{brk,{2,1}},{del,{1,4}}}} = ot({del,{1,4}}, {brk,{2,1}}).
% All nodes are the same
del_brk4_test() ->
    {warn,{nop,{del,{1,4}}}} = ot({del,{1,4}}, {brk,{1,1}}).
% Both break nodes are the same but the delete one is different
del_brk5_test() ->
    {warn,{nop,{del,{2,4}}}} = ot({del,{2,4}}, {brk,{1,1}}).
