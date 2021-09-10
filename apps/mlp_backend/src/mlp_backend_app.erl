%%%-------------------------------------------------------------------
%% @doc MLP-Backend public API
%% @end
%%%-------------------------------------------------------------------

-module(mlp_backend_app).

-behaviour(application).

-export([start/2, stop/1, startServer/0]).

start(_StartType, _StartArgs) ->
   mlp_backend_sup:start_link(),
   startServer().

stop(_State) ->
    ok.

startServer() ->
   {ok,Host    } = application:get_env(mlp_backend,host     ),
   {ok,Port    } = application:get_env(mlp_backend,port     ),
   {ok,NetOpts } = application:get_env(mlp_backend,netopts  ),
   mlp_api_server:start('MPL-Backend', #{ ip => Host, port => Port, net_opts => NetOpts }).
%% internal functions
