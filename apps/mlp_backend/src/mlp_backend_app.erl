%%%-------------------------------------------------------------------
%% @doc MLP-Backend public API
%% @end
%%%-------------------------------------------------------------------

-module(mlp_backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
   mlp_backend_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
