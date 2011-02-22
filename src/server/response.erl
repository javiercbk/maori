%%%-------------------------------------------------------------------
%%% File    : response.erl
%%% Author  : Javier Lecuona <javierlecuona@gmail.com>
%%% Description : response functions for maori
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
-module(response).
-export([ok_response/2,warning_response/2]).

ok_response(OperationClient, OperationServer)->
    {ok,{OperationClient, OperationServer}}.

warning_response(OperationClient, OperationServer)->
    {warn,{OperationClient, OperationServer}}.
