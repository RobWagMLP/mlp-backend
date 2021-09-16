%% @doc RFC 7517: JSON Web Key (JWK)

-module(jwk).
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-export([encode/2, decode/2]).

-type id()         :: binary().
-type public_key() :: #'RSAPublicKey'{} | pem().
-type pem()        :: binary().
-type json()       :: binary().

-spec encode(id(), public_key()) -> {ok, json()} | {error, _}.
%% @doc encode Erlang/OTP Key to JWK
encode(Id, #'RSAPublicKey'{modulus = N, publicExponent = E}) ->
    {ok, jsx:encode(
        #{
            keys =>
            [
                #{
                    kid => Id,
                    kty => <<"RSA">>,
                    n   => encode_int(N),
                    e   => encode_int(E)
                }
            ]
        }
    )};
encode(Id, PEM) when is_binary(PEM) ->
    [RSAEntry] = public_key:pem_decode(PEM),
    encode(Id, public_key:pem_entry_decode(RSAEntry));
encode(_, _) ->
    {error, not_supported}.

-spec decode(id(), json()) -> {ok, public_key()} | {error, _}.
%% @doc decode JWK to Erlang/OTP Key
decode(Id, #{<<"keys">> := JWTs}) ->
    decode(
        lists:dropwhile(
            fun(X) ->
                maps:get(<<"kid">>, X, undefined) /= Id
            end,
            JWTs
        )
    );
decode(Id, Json) when is_binary(Json) ->
    decode(Id, jsx:decode(Json, [return_maps])).

%% @private
decode([#{<<"kty">> := <<"RSA">>, <<"n">> := N, <<"e">> := E} | _]) ->
    {ok,
        #'RSAPublicKey'{
            modulus        = decode_int(N),
            publicExponent = decode_int(E)
        }
    };
decode([]) ->
    {error, not_found};
decode(_) ->
    {error, not_supported}.


%% @private
encode_int(X) ->
    base64url:encode(binary:encode_unsigned(X)).

%% @private
decode_int(X) ->
    binary:decode_unsigned(base64url:decode(X)).
