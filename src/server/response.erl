%%%-------------------------------------------------------------------
%%% File    : response.erl
%%% Author  : Javier Lecuona <chuby@bones>
%%% Description : 
%%%
%%% Created : 13 Feb 2011 by Javier Lecuona <chuby@bones>
%%%-------------------------------------------------------------------
-module(response).
-export([ok_response/2,warning_response/2])

ok_response(OperationClient, OperationServer)->
    {ok,{OperationClient, OperationServer}}.

%%is this the right way to signal a warning???
warning_response(OperationClient, OperationServer)->
    {warn,{OperationClient, OperationServer}}.
