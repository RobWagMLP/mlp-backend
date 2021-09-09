-module(email_gateway).

-export([send_validation_mail/3]).

send_validation_mail(Sender, Receiver, Content) ->
    {Relay, User, PW } = getConf(),
    gen_smtp_client:send({Sender, [Receiver],  Content},
   [{relay, Relay}, {username, User}, {password, PW}]).


getConf() ->
   {ok,Relay}   = application:get_env(email_gateway,relay     ),
   {ok,User }   = application:get_env(email_gateway,user      ),
   {ok,PW }     = application:get_env(email_gateway,password  ),
   {Relay, User, PW}.