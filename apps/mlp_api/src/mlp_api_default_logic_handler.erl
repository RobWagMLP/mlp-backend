-module(mlp_api_default_logic_handler).

-behaviour(mlp_api_logic_handler).

-export([handle_request/3]).
-export([authorize_api_key/2]).

-define(free_operations, ['UserVerify', 'UserCreate', 'UserLogin', 'JwtGet']).

-spec authorize_api_key(OperationID :: mlp_api_api:operation_id(), ApiKey :: binary()) -> {true, #{}}.

authorize_api_key(_, _) -> {true, #{}}.

-spec handle_request(
    OperationID :: mlp_api_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.

%%cookies => [#{name => <<"token">>,value => Token,opts => #{path => "/",http_only => true, secure => true} } ]


handle_request('JwtGet', Context, _) ->
    [Cookie, Token] = create_cookie_and_token(exp_free),
    {200, #{<<"Set-Cookie">> => Cookie}, #{token => Token}};

handle_request('UserGet', Context, #{user_name := UserName}) ->
    case dbconnect:call_sp(db,sp_get_user,#{user_name => UserName}, [true, strip_nulls]) of
        {ok, <<"result">>, Res}         -> {200, #{}, jsx:decode(Res)};
        {error, ErrorCode,ErrorText}    -> {400,#{},#{error_code => ErrorCode,error_text => ErrorText}}
        end;

handle_request('UserVerify', Context, #{'UserVerify' := Params}) ->
    case dbconnect:call_sp(db,sp_user_verify,Params, true ) of
        {ok, <<"result">>, _}           -> {200, #{}, #{result => ok}};
        {error, ErrorCode,ErrorText}    -> {400,#{},#{error_code => ErrorCode,error_text => ErrorText}}
        end;

handle_request('UserCreate', Context, #{'NewUser' := #{<<"username">> := UserName, <<"email_address">> := EmailAddress} } ) ->
    case dbconnect:call_sp(db,sp_create_user, #{user_name => UserName, email_address => EmailAddress}, true) of
        {ok, <<"result">>, Res } -> ParsedRes = jsx:decode(Res, [return_maps]), 
                                    UserID = maps:get(<<"user_id">>, ParsedRes),
                                    Token = maps:get(<<"verification_code">>, ParsedRes),
                                    email_worker:build_validation_template(UserName, EmailAddress, Token),
                                    {200, #{}, #{ user_id => UserID } };
        {error, ErrorCode,ErrorText} -> {400,#{},#{error_code => ErrorCode,error_text => ErrorText}}
        end;

handle_request('UserLogin', Context, #{'UserLogin' := #{<<"user_name">> := UserName, <<"password">> := _PW } = Params}) ->
    case dbconnect:call_sp(db,sp_user_login,Params, true ) of
        {ok, <<"result">>, _}           ->  [Cookie, Token] = create_cookie_and_token(exp_log, UserName),
                                            {200, #{<<"Set-Cookie">> => Cookie}, #{token => Token}};
        {error, ErrorCode,ErrorText}    -> {400,#{},#{error_code => ErrorCode,error_text => ErrorText}}
        end;


handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
        "Got not implemented request to process: ~p~n",
        [{OperationID, Req, Context}]
    ),
    {501, #{}, #{}}.

%%%%%%%%%%%%%%%%%
%%% Helper
%%% %%%%%%%%%%%%

create_cookie_and_token(Type, UserName) ->
    OS_Time          = os:system_time(seconds),
    {ok, ExpTime }   = application:get_env(jwt, Type), 
    {ok, Alg    }    = application:get_env(jwt, alg),
    {ok, Key    }    = application:get_env(jwt, key),
    ExPTotal         = OS_Time + ExpTime,
    Token            = jwerl:sign(#{user_name => UserName, exp => ExPTotal}, Alg, Key),
    Cookie           = <<"jwt=", Token/binary, "; HttpOnly">>,
    [Cookie, Token].


create_cookie_and_token(Type) ->
    OS_Time          = os:system_time(seconds),
    {ok, ExpTime }   = application:get_env(jwt, Type), 
    {ok, Alg    }    = application:get_env(jwt, alg),
    {ok, Key    }    = application:get_env(jwt, key),
    ExPTotal         = OS_Time + ExpTime,
    Token            = jwerl:sign(#{operations => ?free_operations, exp => ExPTotal}, Alg, Key),
     Cookie           = <<"jwt=", Token/binary, "; HttpOnly">>,
    [Cookie, Token].