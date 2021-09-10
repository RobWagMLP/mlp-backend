-module(email_worker).

-export([build_validation_template/3]).
-export ([getMailsToSend/1]).



getMailsToSend(MaxAmount) ->
    ok.

build_validation_template(Username, EmailAdress, Token) ->
    case dbconnect:call_sp(db,get_verfication_template, #{user_name => Username, email_address => EmailAdress, verification_code => Token}, jsonagg) of
        {ok, <<"result">>, Res }  -> [Params] = jsx:decode(Res, [return_maps]),
                                     email_gateway:send_mail(EmailAdress, maps:get(<<"template_text">>, Params), maps:get(<<"subject">>, Params), maps:get(<<"type">>, Params));
        _                         -> {error_sending_email}
    end.

