-module(mlp_api_logic_handler).

-export([handle_request/4]).
-type context() :: #{binary() => any()}.
-type handler_response() ::{
    Status :: cowboy:http_status(),
    Headers :: cowboy:http_headers(),
    Body :: #{}
}.

-define(alg, application:get_env(jwt, alg)).
-define(key, application:get_env(jwt, key)).
-export_type([handler_response/0]).



-callback handle_request(OperationID :: mlp_api_api:operation_id(), cowboy_req:req(), Context :: context()) ->  
    handler_response().

-spec handle_request(
    Handler :: atom(),
    OperationID :: mlp_api_api:operation_id(),
    Request :: cowboy_req:req(),
    Context :: context()
) ->
    handler_response().

handle_request(Handler, OperationID, Req, Context) ->
    case handleAuth(Req, OperationID, Context) of 
        true -> Handler:handle_request(OperationID, Req, Context);
        _    -> {401, #{}, #{error => authentication_error}}
    end.


handleAuth(#{ headers := #{ <<"authorization">> := Bearer }}, OperationID, Context ) ->
    JWT = iolist_to_binary(re:replace(Bearer, "Bearer ", "")),
    {ok, Alg    }    = application:get_env(jwt, alg),
    {ok, Key    }    = application:get_env(jwt, key),
    case jwerl:verify(JWT, Alg, Key) of
        {ok, Params} -> error_logger:info_msg(Params),
                        handle_jwt(Params, OperationID, Context);
        _            -> handle_jwt(null, OperationID, null )
    end;

handleAuth(#{ headers := #{ <<"cookie">> := Cookie } }, OperationID, Context ) ->
    [Name, JWTStr] = string:split(Cookie, "="),
    error_logger:info_msg({Name, JWTStr, Name}),
    case Name of 
        <<"jwt">> -> JWT = iolist_to_binary(JWTStr),
                    {ok, Alg    }    = application:get_env(jwt, alg),
                    {ok, Key    }    = application:get_env(jwt, key),
                    case jwerl:verify(JWT, Alg, Key) of
                        {ok, Params} -> error_logger:info_msg(Params),
                                        handle_jwt(Params, OperationID, Context);
                        _            -> handle_jwt(null, OperationID, null )
                    end
    end;

handleAuth(_, OperationID, _) ->
    handle_jwt(null, OperationID, null).


handle_jwt(#{ operations := Operations, exp := TS}, OperationID, _) ->
    case lists:member(atom_to_binary(OperationID, unicode), Operations) of
        true -> check_exp_date(TS, OperationID);
        _    -> handle_jwt(null, OperationID, null )
    end;

handle_jwt(#{ user_name := User_Name, exp := TS}, OperationID, #{user_name := User_Name_Req}) ->
    case User_Name of 
        User_Name_Req -> check_exp_date(TS, OperationID);
        _             -> handle_jwt(null, OperationID, null )
    end;

handle_jwt(_ ,'JwtGet' , _) ->
    true;

handle_jwt(_ ,_ ,__) ->
    false.

check_exp_date(TS, OperationID) ->
    CompData = os:system_time(seconds),
    case CompData < TS of
        true -> true;
        _    -> handle_jwt(null, OperationID, null )
    end.