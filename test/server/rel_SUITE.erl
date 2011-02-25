%%%-------------------------------------------------------------------
%%% File    : rel_SUITE.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : relate test suite
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
-module(rel_SUITE).
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
rel_nop1_test() ->
    {ok,{nop,{rel,{1,2}}}} = ot({rel,{1,2}}, nop).
% Same nodes
rel_nop2_test() ->
    {warn,{nop,nop}} = ot({rel,{1,1}}, nop).

%%----------------------
%% rel with ins test
%%----------------------
% Different nodes
rel_ins1_test() ->
    {ok,{{ins,{1,2}},{rel,{3,4}}}} = ot({rel,{3,4}},{ins,{1,2}}).
% Same first node
rel_ins2_test() ->
    {ok,{{ins,{1,2}},{rel,{1,4}}}} = ot({rel,{1,4}},{ins,{1,2}}).
% Same second node
rel_ins3_test() ->
    {ok,{{ins,{1,2}},{rel,{3,1}}}} = ot({rel,{3,1}},{ins,{1,2}}).
% All nodes are the same
rel_ins4_test() ->
    {warn,{{ins,{1,2}},nop}} = ot({rel,{1,1}},{ins,{1,2}}).
% Both break nodes are the same but insert node is different
rel_ins5_test() ->
    {warn,{{ins,{1,2}},nop}} = ot({rel,{3,3}},{ins,{1,2}}).

%%----------------------
%% rel with del test
%%----------------------
% Different nodes
rel_del1_test() ->
    {ok,{{del,{1,2}},{rel,{3,4}}}} = ot({rel,{3,4}},{del,{1,2}}).
% Same first node
rel_del2_test() ->
    {ok,{{del,{1,2}},{rel,{1,4}}}} = ot({rel,{1,4}},{del,{1,2}}).
% Same second node
rel_del3_test() ->
    {ok,{{del,{1,2}},{rel,{3,1}}}} = ot({rel,{3,1}},{del,{1,2}}).
% All nodes are the same
rel_del4_test() ->
    {warn,{{del,{1,2}},nop}} = ot({rel,{1,1}},{del,{1,2}}).
% Both break nodes are the same but del node is different
rel_del5_test() ->
    {warn,{{del,{1,2}},nop}} = ot({rel,{3,3}},{del,{1,2}}).

%%----------------------
%% rel with rel test
%%----------------------
% break and relate same relationship
rel_rel0_test() ->
    {ok,{nop,nop}} = ot({rel,{1,2}}, {rel,{1,2}}).
%All nodes are different
rel_rel1_test() ->
    {ok,{{rel,{3,4}},{rel,{1,2}}}} = ot({rel,{1,2}}, {rel,{3,4}}).
% First node of rel node is the First node of rel. Other are different
rel_rel2_test() ->
    {ok,{{rel,{1,3}},{rel,{1,2}}}} = ot({rel,{1,2}}, {rel,{1,3}}).
% First node of rel node is the Second node of rel. Other are different
rel_rel3_test() ->
    {ok,{{rel,{1,3}},{rel,{2,1}}}} = ot({rel,{2,1}}, {rel,{1,3}}).
% Second node of rel node is the First node of rel. Other are different
rel_rel4_test() ->
    {ok,{{rel,{3,1}},{rel,{1,2}}}} = ot({rel,{1,2}}, {rel,{3,1}}).
% Second node of rel node is the Second node of rel. Other are different
rel_rel5_test() ->
    {ok,{{rel,{3,1}},{rel,{2,1}}}} = ot({rel,{2,1}}, {rel,{3,1}}).
% Both of rel nodes are the First node of rel. Other are different
rel_rel6_test() ->
    {warn,{nop,{rel,{1,2}}}} = ot({rel,{1,2}}, {rel,{1,1}}).
% Both of rel nodes are the Second node of rel. Other are different
rel_rel7_test() ->
    {warn,{nop,{rel,{2,1}}}} = ot({rel,{2,1}}, {rel,{1,1}}).
% All nodes are the same
rel_rel8_test() ->
    {warn,{nop,nop}} = ot({rel,{1,1}}, {rel,{1,1}}).

%%----------------------
%% brk with brk test
%%----------------------
% Break and break same relationship
rel_brk0_test() ->
    {ok,{nop,{rel,{1,2}}}} = ot({rel,{1,2}}, {brk,{1,2}}).
%All nodes are different
rel_brk1_test() ->
    {ok,{{brk,{3,4}},{rel,{1,2}}}} = ot({rel,{1,2}}, {brk,{3,4}}).
% First node of brk node is the First node of rel. Others are different
rel_brk2_test() ->
    {ok,{{brk,{1,3}},{rel,{1,2}}}} = ot({rel,{1,2}}, {brk,{1,3}}).
% First node of brk node is the Second node of rel. Others are different
rel_brk3_test() ->
    {ok,{{brk,{1,3}},{rel,{2,1}}}} = ot({rel,{2,1}}, {brk,{1,3}}).
% Second node of brk node is the First node of rel. Others are different
rel_brk4_test() ->
    {ok,{{brk,{3,1}},{rel,{1,2}}}} = ot({rel,{1,2}}, {brk,{3,1}}).
% Second node of brk node is the Second node of rel. Others are different
rel_brk5_test() ->
    {ok,{{brk,{3,1}},{rel,{2,1}}}} = ot({rel,{2,1}}, {brk,{3,1}}).
% Both of brk nodes are the First node of rel. Others are different
rel_brk6_test() ->
    {warn,{nop,{rel,{1,2}}}} = ot({rel,{1,2}}, {brk,{1,1}}).
% Both of brk nodes are the Second node of rel. Others are different
rel_brk7_test() ->
    {warn,{nop,{rel,{2,1}}}} = ot({rel,{2,1}}, {brk,{1,1}}).
% All nodes are the same
rel_brk8_test() ->
    {warn,{nop,nop}} = ot({rel,{1,1}}, {brk,{1,1}}).
