-module(mlp_api_default_logic_handler).

-behaviour(mlp_api_logic_handler).

-export([handle_request/3]).
-export([authorize_api_key/2]).
-export([authorize_api_key/2]).

-spec authorize_api_key(OperationID :: mlp_api_api:operation_id(), ApiKey :: binary()) -> {true, #{}}.

authorize_api_key(_, _) -> {true, #{}}.
-spec authorize_api_key(OperationID :: mlp_api_api:operation_id(), ApiKey :: binary()) -> {true, #{}}.

authorize_api_key(_, _) -> {true, #{}}.

-spec handle_request(
    OperationID :: mlp_api_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.




handle_request('PersonGet', #{}, Context) ->
    dbconnection:call_proc(db,sp_access_key_get_active,#{access_key => Access_Key}, plain)
    logger:info('Hello');

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
        "Got not implemented request to process: ~p~n",
        [{OperationID, Req, Context}]
    ),
    {501, #{}, #{}}.
