-module(email_worker).

-export([build_validation_template/1]).
-export ([getMailsToSend/1]).



getMailsToSend(MaxAmount) ->


build_validation_template(Username, EmailAdress, Sender) ->
