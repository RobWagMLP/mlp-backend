%%%-------------------------------------------------------------------
%% @doc MLP-Backend public API
%% @end
%%%-------------------------------------------------------------------

-module(MLP-Backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    MLP-Backend_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
