-module(mlp_api_default_logic_handler).

-behaviour(mlp_api_logic_handler).

-export([handle_request/3]).
-export([authorize_api_key/2]).

-spec authorize_api_key(OperationID :: mlp_api_api:operation_id(), ApiKey :: binary()) -> {true, #{}}.

authorize_api_key(_, _) -> {true, #{}}.

-spec handle_request(
    OperationID :: mlp_api_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.




handle_request('UserGet', Context, #{user_name := UserName}) ->
    case dbconnect:call_sp(db,sp_get_user,#{user_name => UserName}, [true, strip_nulls]) of
        {ok, <<"result">>, Res} -> {200, #{}, jsx:decode(Res)};
         _                      -> {400, #{}, #{}}
        end;


handle_request('UserCreate', Context, #{'NewUser' := Params } ) ->
    error_logger:error_msg("HANS"),
    case dbconnect:call_sp(db,sp_create_user, Params, true) of
        {ok, <<"result">>, Res} -> {200, #{}, #{ user_id => binary_to_integer(Res) } };
         _                      -> {400, #{}, #{}}
        end;

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
        "Got not implemented request to process: ~p~n",
        [{OperationID, Req, Context}]
    ),
    {501, #{}, #{}}.

