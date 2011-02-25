%%%-------------------------------------------------------------------
%%% File    : nop_SUITE.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : no operation test SUITE
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
-module(nop_SUITE).
-include_lib("eunit/include/eunit.hrl").
%% Note: This directive should only be used in test suites.
%%-compile(export_all).
-import(protocol,[ot/2]).

%%----------------------
%% protocol nop test
%%----------------------
simple_nop_test() -> 
    ?_assertEqual({ok, {nop, nop}}, ot(nop, nop)).
nop_ins1_test() -> 
    {ok, {{ins,{1,2}}, nop}} = ot(nop, {ins,{1,2}}).
nop_ins2_test() -> 
    {ok, {{ins,{1,1}}, nop}} = ot(nop, {ins,{1,1}}).
nop_del1_test() -> 
    {ok, {{del,{1,1}}, nop}} = ot(nop, {del,{1,1}}).
nop_del2_test() -> 
    {ok, {{del,{1,2}}, nop}} = ot(nop, {del,{1,2}}).
nop_brk1_test() ->
    {ok,{{brk,{1,2}},nop}} = ot(nop,{brk,{1,2}}).
nop_brk2_test() ->
    {warn, {nop, nop}} = ot(nop,{brk,{1,1}}).
nop_rel1_test() ->
    {ok,{rel,{1,2}},nop} = ot(nop,{rel,{1,2}}).
nop_rel2_test() ->
    {warn, {nop, nop}} = ot(nop,{rel,{1,1}}).
