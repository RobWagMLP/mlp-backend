-module(dbconnect_worker).
-author("mschoeneich").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("dbconnection.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-behaviour(gen_server).
-behaviour(poolboy_worker).


-compile(inline).
-compile({inline_size,100}).
-compile(inline_list_funcs).


-define(STATEMENT_TIMEOUT,10000).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(state, {conn, prepstmt,sprocs= #{}}).

-type statementmap():: #{{spname(),paramlist(),spoption()} => epgsql:statement()}.




start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).



init(ConnectionOptions) ->
    %error_logger:info_msg("start worker ~p",[self()]),
    %process_flag(trap_exit, true),
    {ok, Conn} = epgsql:connect(ConnectionOptions),
    {ok,_,_}             =epgsql:equery( Conn,io_lib:format("set statement_timeout = ~p",[?STATEMENT_TIMEOUT])),
    {ok,_,_}             =epgsql:equery( Conn,"set plan_cache_mode = force_generic_plan"),
    {ok, #state{conn = Conn,prepstmt = #{}}}.

handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call({prepared_query, Stmt, Params}, _From, #state{conn = Conn, prepstmt = PreparedStatements} = State) ->
   
    case maps:is_key(Stmt,State#state.prepstmt) of
        true    -> Statement = maps:get(Stmt,State#state.prepstmt),
                   NewState = State,
                  {reply, prepared_query(Conn, Statement, Params), NewState};
        false   -> StatementId    = generate_unique_statement_name(),
                   case epgsql:parse(Conn,StatementId,Stmt,[]) of
                       {ok   ,Statement} -> NewState       = State#state{prepstmt=PreparedStatements#{Stmt => Statement}},
                                            {reply, prepared_query(Conn, Statement, Params), NewState};
                       Error             -> {reply, Error                                  , State}
                   end
    end;
handle_call({call_proc, SpName, Params,Values,Option},_From,#state{conn = Conn, sprocs = Sprocs} = State) ->
    % achtung hier umgehen wir wieder die epgsql api, denn da geht stand epgsql 3.x der name rein, sodass für jedes ausführen
    % zweimal zur db gerannt wird. einmal fürs discribe und dann fürs execute
    {ok, #statement{types = Types} = Sproc,NewSprocs} = get_or_parse_sproc(Conn,SpName, Params,Option,Sprocs),
    Typed_Values = lists:zip(Types, Values),
    {reply,gen_server:call(Conn, {prepared_query, Sproc, Typed_Values}, infinity),State#state{sprocs=NewSprocs}};

handle_call({call_batch_proc, SpsWithSeparatedParamsValuesOption},_From,#state{conn = Conn, sprocs = Sprocs} = State) ->
     {ok,PreparedBatch,NewSprocs,Columns} = prepare_batch(Conn,SpsWithSeparatedParamsValuesOption,Sprocs),
     {reply,{{results,epgsql:execute_batch(Conn,PreparedBatch)},{columns,Columns}},State#state{sprocs=NewSprocs}};

handle_call({update_type_cache,DynamicTypes}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:update_type_cache(Conn,DynamicTypes), State};

handle_call({close_prepared_query,Conn, Stmt}, _From, #state{conn = Conn, prepstmt = PreparedStatements} = State) ->
    {ok} = gen_server:call(Conn, {close, statement, Stmt}),
    NewState = State#state{prepstmt=maps:remove(Stmt,PreparedStatements)},
    {reply, ok,NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Private API

generate_unique_statement_name() ->
    % postgres objektnamen sind maximal 63 zeichen lang, wir müssen uns daher einen eindeutigen namen für unsere prepared statements ausdenken
    % verwenden dafür den eingebauten id generator
    io_lib:format("~p", [erlang:unique_integer([monotonic])]).


%das interface epgsql:prepared_query ist murks, denn im bind holen wir uns bereits die komplette struktur ab,
%um sie zu vergessen. und sie beim aufruf von prepared query von der db per namen wieder abzufragen. sprich jeder prozeduraufruf sind zwei dbaufrufe...
prepared_query(Connection,Statement,Parameters) ->
    
    #statement{types = Types} = Statement,
    Typed_Parameters = lists:zip(Types, Parameters),
    gen_server:call(Connection, {prepared_query, Statement, Typed_Parameters}, infinity).


% eigentlich müssten wir die bereits in der db geparsten statements in einem eigenen prozess halten
% falls nämlich nach dem parsen ein fehler auftritt. sei es beim parsen des nächsten statements oder dann beim ausführen
% so werden wir zurückgerollt - die db aber nicht! damit akkumulieren wir dort dann ewig viele statements
-spec get_or_parse_sproc(epgsql:connection(),spname(),paramlist(),spoption(),statementmap()) -> {ok,statementmap()}|{error,epgsql:error()}.
get_or_parse_sproc(Conn,SpName, Params,Option,Sprocs) ->
    %schritt 1 sehen nach ob wir die kombination aus sp, parameter und Option bereits kennen
    SprocStatement = maps:get({SpName, Params,Option},Sprocs,undefined),
    case SprocStatement of 
        undefined -> StatementId = generate_unique_statement_name(),
                     StatementString = generate_statement_string(SpName,Params,Option),
                     case epgsql:parse(Conn,StatementId,StatementString,[]) of 
                         {ok,NewSprocStatement} -> {ok,NewSprocStatement,Sprocs#{{SpName, Params,Option} => NewSprocStatement}};
                         {error,Error}          -> {error,Error}
                     end;
        _         -> {ok, SprocStatement,Sprocs}
    end.

-spec prepare_batch(epgsql:connection(),list(sp_param_values_option()),statementmap()) -> {ok,batch(),statementmap(),list()}.
prepare_batch(Conn,SpsWithSeparatedParamsValuesOption,Sprocs) ->
    RevSpsWithSeparatedParamsValuesOption = lists:reverse(SpsWithSeparatedParamsValuesOption),
    prepare_batch(Conn,RevSpsWithSeparatedParamsValuesOption,Sprocs,[],[]).  
  
prepare_batch(_Conn,[]=_SpsWithSeparatedParamsValuesOption,Sprocs,PreparedBatch,Columns) ->
   {ok,PreparedBatch,Sprocs,Columns};
prepare_batch(Conn,[{SpName,{Params,Values},Option}|T]=_SpsWithSeparatedParamsValuesOption,Sprocs,PreparedBatch,Columns) ->
  {ok,  Sproc,NewSprocs} = get_or_parse_sproc(Conn,SpName, Params,Option,Sprocs),
  NewPreparedBatch  = [{Sproc,Values}|PreparedBatch],
  NewColumns        = [Sproc#statement.columns|Columns],
  prepare_batch(Conn,T,NewSprocs,NewPreparedBatch,NewColumns).

-spec generate_statement_string(spname(),paramlist(),spoption()) -> binary().
generate_statement_string (SpName,Params,Options) when is_atom(SpName)-> 
    SpNameBin = atom_to_binary(SpName,unicode),
    generate_statement_string_bin (SpNameBin,Params,Options );
generate_statement_string (SpName,Params,Options) when is_list(SpName)-> 
    SpNameBin = list_to_binary(SpName),
    generate_statement_string_bin (SpNameBin,Params,Options );
generate_statement_string (SpName,Params,Options) when is_binary(SpName)-> 
    SpNameBin = SpName,
    generate_statement_string_bin (SpNameBin,Params,Options ).

% specbinary,list,Option -> binary
-spec generate_statement_string_bin(binary(),paramlist(),spoption()) ->binary().
generate_statement_string_bin (SpName,Params,plain ) ->
    PreparedParams = prepare_params(Params),
    <<"select a.* from ",SpName/binary,"(",PreparedParams/binary,") as a">>;
generate_statement_string_bin (SpName,Params,json ) -> 
    PreparedParams = prepare_params(Params),
    <<"select to_json(a.*) as result from ",SpName/binary,"(",PreparedParams/binary,") as a">>;
generate_statement_string_bin (SpName,Params,json_agg ) -> 
    PreparedParams = prepare_params(Params),
    <<"select coalesce(json_agg(a),'[]') as result from ",SpName/binary,"(",PreparedParams/binary,") as a">>;
generate_statement_string_bin (SpName,Params,json_strip_nulls ) -> 
    PreparedParams = prepare_params(Params),
    <<"select coalesce(json_strip_nulls(to_json(a)),'{}') as result from ",SpName/binary,"(",PreparedParams/binary,") as a">>;
generate_statement_string_bin (SpName,Params,jsonagg_strip_nulls ) -> 
    PreparedParams = prepare_params(Params),
    <<"select coalesce(json_strip_nulls(json_agg(a)),'[]') as result from ",SpName/binary,"(",PreparedParams/binary,") as a">>.






-spec prepare_params(paramlist()) -> binary().
prepare_params(Params) ->
    %ReverseParams = lists:reverse(Params),
    prepare_params(Params,<<>>,1).

%spec list,binary,pos -> binary|list,binary,pos

-spec prepare_params(paramlist(),binary(),integer()) -> binary().
prepare_params([],Accum,_Pos) ->
    Accum;
prepare_params([H|T],<<>>,1 = Pos) when is_atom(H)->
    Hb = atom_to_binary(H,unicode),
    PosBin = integer_to_binary(Pos),
    prepare_params(T,<<Hb/binary,":=$",PosBin/binary>>,Pos+1);
prepare_params([H|T],<<>>,1= Pos) when is_list(H)->
    Hb = list_to_binary(H),
    PosBin = integer_to_binary(Pos),
    prepare_params(T,<<Hb/binary,":=$",PosBin/binary>>,Pos+1);
prepare_params([H|T],<<>>,1= Pos) when is_binary(H)->
    Hb = H,
    PosBin = integer_to_binary(Pos),
    prepare_params(T,<<Hb/binary,":=$",PosBin/binary>>,Pos+1);
 
prepare_params([H|T],Accum, Pos) when is_atom(H)->
    Hb = atom_to_binary(H,unicode),
    PosBin = integer_to_binary(Pos),
    prepare_params(T,<<Accum/binary,",",Hb/binary,":=$",PosBin/binary>>,Pos+1);
prepare_params([H|T],Accum, Pos) when is_list(H)->
    Hb = list_to_binary(H),
    PosBin = integer_to_binary(Pos),
    prepare_params(T,<<Accum/binary,",",Hb/binary,":=$",PosBin/binary>>,Pos+1);
prepare_params([H|T],Accum, Pos) when is_binary(H)->
    Hb = H,
    PosBin = integer_to_binary(Pos),
    prepare_params(T,<<Accum/binary,",",Hb/binary,":=$",PosBin/binary>>,Pos+1).



%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(EUNIT).

prepare_params_test() ->
    Params = [ato,"str",<<"bin">>],
    %ggf reihenfolge anpassen 
    ?assertEqual(<<"ato:=$1,str:=$2,bin:=$3">>,prepare_params(Params)).

generate_statement_string_plain_test () -> 
    SpName = sp_test,
    Params = [ato,"str",<<"bin">>],
    ?assertEqual(<<"select a.* from sp_test(ato:=$1,str:=$2,bin:=$3) as a">>,generate_statement_string (SpName,Params,plain)).
generate_statement_string_json_test () -> 
    SpName = sp_test,
    Params = [ato,"str",<<"bin">>],
    ?assertEqual(<<"select to_json(a.*) as result from sp_test(ato:=$1,str:=$2,bin:=$3) as a">>,generate_statement_string (SpName,Params,json)).
generate_statement_string_json_agg_test () -> 
    SpName = sp_test,
    Params = [ato,"str",<<"bin">>],
    ?assertEqual(<<"select coalesce(json_agg(a),'[]') as result from sp_test(ato:=$1,str:=$2,bin:=$3) as a">>,generate_statement_string (SpName,Params,json_agg)).
generate_statement_string_json_strip_nulls_test ( ) -> 
    SpName = sp_test,
    Params = [ato,"str",<<"bin">>],
    ?assertEqual(<<"select coalesce(json_strip_nulls(to_json(a)),'{}') as result from sp_test(ato:=$1,str:=$2,bin:=$3) as a">>,generate_statement_string (SpName,Params,json_strip_nulls)).
generate_statement_string_jsonagg_strip_nulls_test ( ) -> 
    SpName = sp_test,
    Params = [ato,"str",<<"bin">>],
    ?assertEqual(<<"select coalesce(json_strip_nulls(json_agg(a)),'[]') as result from sp_test(ato:=$1,str:=$2,bin:=$3) as a">>,generate_statement_string (SpName,Params,jsonagg_strip_nulls)).
-endif.
