%%%-------------------------------------------------------------------
%%% File    : protocol_SUITE.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : 
%%%
%%% Created : 12 Feb 2011 by Javier Lecuona <chuby@bones>
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
-module(protocol_SUITE).
-include_lib("eunit/include/eunit.hrl").
%% Note: This directive should only be used in test suites.
-compile(export_all).

-import(protocol,[ot/2]).


simple_nop_test() -> 
    {ok, {nop, nop}} = ot(nop, nop).
