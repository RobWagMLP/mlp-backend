-module(email_gateway).

-export([send_mail/4]).



send_mail(Receiver, Content, Subject , ContentType) ->
    {Relay, User, PW } = getConf(),
    TextContentType = <<"plain">>,
    Mimemail =          {<<"text">>,             ContentType,
                       [{<<"From">>             , erlang:iolist_to_binary(["MLPfun"," <",User,">"])}, % "ndPay <"++Sender++">"
                        {<<"Reply-To">>         , <<"robert.wagner@ndbit.de">>},
                        {<<"To">>               , [Receiver]},
                        {<<"Subject">>          , <<Subject/binary>>},
                        {<<"Content-Type">>     , <<"text/",ContentType/binary,"; charset=utf-8">>}],
                       [{<<"transfer-encoding">>, <<"quoted-printable">>}],
                       Content},
            MM = mimemail:encode(Mimemail),
    gen_smtp_client:send({User, [Receiver],  MM},
   [{relay, Relay}, {username, User}, {password, PW}]).


getConf() ->
   {ok,Relay}   = application:get_env(email_gateway,relay     ),
   {ok,User }   = application:get_env(email_gateway,user      ),
   {ok,PW }     = application:get_env(email_gateway,password  ),
   {Relay, User, PW}.
