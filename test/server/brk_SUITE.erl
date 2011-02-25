%%%-------------------------------------------------------------------
%%% File    : brk_SUITE.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : break test suite
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
-module(brk_SUITE).
-include_lib("eunit/include/eunit.hrl").
%% Note: This directive should only be used in test suites.
%%-compile(export_all).
-import(protocol,[ot/2]).

%%----------------------
%% protocol brk test
%%----------------------
%% brk with nop test
%%----------------------
% Different nodes
brk_nop1_test() ->
    {ok,{nop,{brk,{1,2}}}} = ot({brk,{1,2}}, nop).
% Same nodes
brk_nop2_test() ->
    {warn,{nop,nop}} = ot({brk,{1,1}}, nop).

%%----------------------
%% brk with ins test
%%----------------------
% Different nodes
brk_ins1_test() ->
    {ok,{{ins,{1,2}},{brk,{3,4}}}} = ot({brk,{3,4}},{ins,{1,2}}).
% Same first node
brk_ins2_test() ->
    {ok,{{ins,{1,2}},{brk,{1,4}}}} = ot({brk,{1,4}},{ins,{1,2}}).
% Same second node
brk_ins3_test() ->
    {ok,{{ins,{1,2}},{brk,{3,1}}}} = ot({brk,{3,1}},{ins,{1,2}}).
% All nodes are the same
brk_ins4_test() ->
    {warn,{{ins,{1,2}},nop}} = ot({brk,{1,1}},{ins,{1,2}}).
% Both break nodes are the same but insert node is different
brk_ins5_test() ->
    {warn,{{ins,{1,2}},nop}} = ot({brk,{3,3}},{ins,{1,2}}).

%%----------------------
%% brk with del test
%%----------------------
% Different nodes
brk_del1_test() ->
    {ok,{{del,{1,2}},{brk,{3,4}}}} = ot({brk,{3,4}},{del,{1,2}}).
% Same first node
brk_del2_test() ->
    {ok,{{del,{1,2}},{brk,{1,4}}}} = ot({brk,{1,4}},{del,{1,2}}).
% Same second node
brk_del3_test() ->
    {ok,{{del,{1,2}},{brk,{3,1}}}} = ot({brk,{3,1}},{del,{1,2}}).
% All nodes are the same
brk_del4_test() ->
    {warn,{{del,{1,2}},nop}} = ot({brk,{1,1}},{del,{1,2}}).
% Both break nodes are the same but del node is different
brk_del5_test() ->
    {warn,{{del,{1,2}},nop}} = ot({brk,{3,3}},{del,{1,2}}).

%%----------------------
%% brk with rel test
%%----------------------
% break and relate same relationship
brk_rel0_test() ->
    {ok,{{rel,{1,2}},nop}} = ot({brk,{1,2}}, {rel,{1,2}}).
%All nodes are different
brk_rel1_test() ->
    {ok,{{rel,{3,4}},{brk,{1,2}}}} = ot({brk,{1,2}}, {rel,{3,4}}).
% First node of rel node is the First node of brk. Other are different
brk_rel2_test() ->
    {ok,{{rel,{1,3}},{brk,{1,2}}}} = ot({brk,{1,2}}, {rel,{1,3}}).
% First node of rel node is the Second node of brk. Other are different
brk_rel3_test() ->
    {ok,{{rel,{1,3}},{brk,{2,1}}}} = ot({brk,{2,1}}, {rel,{1,3}}).
% Second node of rel node is the First node of brk. Other are different
brk_rel4_test() ->
    {ok,{{rel,{3,1}},{brk,{1,2}}}} = ot({brk,{1,2}}, {rel,{3,1}}).
% Second node of rel node is the Second node of brk. Other are different
brk_rel5_test() ->
    {ok,{{rel,{3,1}},{brk,{2,1}}}} = ot({brk,{2,1}}, {rel,{3,1}}).
% Both of rel nodes are the First node of brk. Other are different
brk_rel6_test() ->
    {warn,{nop,{brk,{1,2}}}} = ot({brk,{1,2}}, {rel,{1,1}}).
% Both of rel nodes are the Second node of brk. Other are different
brk_rel7_test() ->
    {warn,{nop,{brk,{2,1}}}} = ot({brk,{2,1}}, {rel,{1,1}}).
% All nodes are the same
brk_rel8_test() ->
    {warn,{nop,nop}} = ot({brk,{1,1}}, {rel,{1,1}}).

%%----------------------
%% brk with brk test
%%----------------------
% Break and break same relationship
brk_brk0_test() ->
    {ok,{nop,nop}} = ot({brk,{1,2}}, {brk,{1,2}}).
%All nodes are different
brk_brk1_test() ->
    {ok,{{brk,{3,4}},{brk,{1,2}}}} = ot({brk,{1,2}}, {brk,{3,4}}).
% First node of brk node is the First node of brk. Others are different
brk_brk2_test() ->
    {ok,{{brk,{1,3}},{brk,{1,2}}}} = ot({brk,{1,2}}, {brk,{1,3}}).
% First node of brk node is the Second node of brk. Others are different
brk_brk3_test() ->
    {ok,{{brk,{1,3}},{brk,{2,1}}}} = ot({brk,{2,1}}, {brk,{1,3}}).
% Second node of brk node is the First node of brk. Others are different
brk_brk4_test() ->
    {ok,{{brk,{3,1}},{brk,{1,2}}}} = ot({brk,{1,2}}, {brk,{3,1}}).
% Second node of brk node is the Second node of brk. Others are different
brk_brk5_test() ->
    {ok,{{brk,{3,1}},{brk,{2,1}}}} = ot({brk,{2,1}}, {brk,{3,1}}).
% Both of brk nodes are the First node of brk. Others are different
brk_brk6_test() ->
    {warn,{nop,{brk,{1,2}}}} = ot({brk,{1,2}}, {brk,{1,1}}).
% Both of brk nodes are the Second node of brk. Others are different
brk_brk7_test() ->
    {warn,{nop,{brk,{2,1}}}} = ot({brk,{2,1}}, {brk,{1,1}}).
% All nodes are the same
brk_brk8_test() ->
    {warn,{nop,nop}} = ot({brk,{1,1}}, {brk,{1,1}}).
