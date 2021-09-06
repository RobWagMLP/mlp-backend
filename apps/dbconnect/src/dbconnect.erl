%%%-------------------------------------------------------------------
%% @doc nbaserl public API
%% @end
%%%-------------------------------------------------------------------

-module(dbconnect).

-behaviour(application).
-behaviour(supervisor).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("dbconnection.hrl").

-define(POOL_TIMEOUT,15000).
-compile(inline).
-compile({inline_size,100}).
-compile(inline_list_funcs).

-export([squery/2,
    equery/3,
    prepared_query/3,
    call_procedure/4,
    call_sp/4,
    call_proc/4,
    call_batch_proc/2,
    to_map/2,
    update_type_cache/2,
    close_prepared_query/2,
    close_procedure/3, handle_x_log_data/4]).

-export([start/0, stop/0,start/2, stop/1,start_link/0]).
-export([init/1]).

-include_lib("epgsql/include/epgsql.hrl").

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, dbconnection}, ?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


stop(_State) ->
    ok.




init([]) ->
    {ok, Pools} = application:get_env(dbconnection, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                              PoolArgs = [{name, {local, Name}},
                              {worker_module, dbconnection_worker}] ++ SizeArgs,
                              poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),
    %PoolSpecs2 = lists:map(fun(Liste) -> setelement(3,Liste,temporary) end,PoolSpecs),
    
    {ok, {#{strategy  =>one_for_one,
            intensity => 10,
            period     => 10}, PoolSpecs}}.

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
                                  end).

equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
                                  end).

prepared_query(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {prepared_query, Stmt, Params},?POOL_TIMEOUT)
                                  end).

close_prepared_query(PoolName, Stmt) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {close_prepared_querys, Stmt})
                                  end).


call_procedure(PoolName,SpName,SpParams,AsJson) ->
    %error_logger:info_msg("spname <~p> SpParams <~p> output json ~p~n",[SpName,SpParams,AsJson]),
    {ok, Params, Values } = prepare_params(SpParams),
    %diese art der parametrisierung ist murks, müssen wir bei gelegenheit schöner machen
    case AsJson of
      false                 -> QueryString = io_lib:format("select a.* from ~s(~s) as a",[SpName,Params]);
        true                  -> QueryString = io_lib:format("select to_json(a) as result from ~s(~s) as a",[SpName,Params]);
      jsonagg               -> QueryString = io_lib:format("select coalesce(json_agg(a),'[]') as result from ~s(~s) as a",[SpName,Params]);
      [true,strip_nulls]    -> QueryString = io_lib:format("select coalesce(json_strip_nulls(to_json(a)),'{}') as result from ~s(~s) as a",[SpName,Params]);
      [jsonagg,strip_nulls] -> QueryString = io_lib:format("select coalesce(json_strip_nulls(json_agg(a)),'[]') as result from ~s(~s) as a",[SpName,Params])
    end,
    prepared_query(PoolName,QueryString,Values).

call_sp(PoolName,SpName,SpParams,AsJson) ->
    case call_procedure(PoolName,SpName,SpParams,AsJson) of
        {ok,[{_,Header,_,_,_,_}|_] ,[{Data}|_] }                                  ->{ok, Header,Data};
        {ok,[{_,Header,_,_,_,_}|_] ,[Data  |_] }                                  ->{ok, Header,Data};
        {ok,[{_,Header,_,_,_,_}|_] ,[] }                                  ->{ok, Header,[]};
        {error,{error,error,_,raise_exception,ErrorCode,[{detail,ErrorText}|_]}}  ->
            %hier hoert mein voodoo auf. ich hab keine ahnung wie ich feststelle, ob ein binary_string ein integer ist, daher kann ich hier keine einfache guard expression anwenden...
            StringErrorCode = binary_to_list(ErrorCode),
            case string:to_integer(StringErrorCode) of
                {error,no_integer} -> {error,9999999  ,<<ErrorCode/binary, ErrorText/binary>>};
                {IntErrorCode  ,_} -> {error,IntErrorCode,ErrorText}
            end;
        {error,{error,error,_SQLState,_,ErrorMessage,[{constraint_name,ConstraintName},
            {detail         ,Detail        }|_]}} ->
            {error,9999999, <<ErrorMessage/binary, ConstraintName/binary, Detail/binary>>}
    end.
-spec call_proc(atom(),spname(),map(),spoption())  -> {ok,list()}.
call_proc(PoolName,SpName,ParamsValues,Option) ->
    {Params,Values} = separate_keys_values(ParamsValues),
%%Start = os:timestamp(),
    Res = poolboy:transaction(PoolName, 
                              fun(Worker) ->
                                gen_server:call(Worker, {call_proc, SpName, Params,Values,Option},?POOL_TIMEOUT)
                              end),                      
%% io:format("total time taken ~f ms sp ~p~n ", [timer:now_diff(os:timestamp(), Start) / 1000,SpName]),
    postprocess_proc_call(Res).
    %Res.

% spec atom,list( SpName,ParamsValues,Option) -> irgendwas
-spec call_batch_proc(atom(),list({spname(),map(),spoption()}))  -> {ok,term()}.
call_batch_proc(PoolName,Sps_ParamValues_Option) ->
    SpsWithSeparatedParamsValuesOption = separate_sp_keys_values_option(Sps_ParamValues_Option),
    Res = poolboy:transaction(PoolName, 
                              fun(Worker) ->
                                gen_server:call(Worker, {call_batch_proc, SpsWithSeparatedParamsValuesOption})                                  
                              end),
    postprocessBatchCall(Res).


close_procedure(PoolName,SpName,SpParams) ->
    error_logger:info_msg("spname <~p> SpParams <~p>~n",[SpName,SpParams]),
    {ok, Params, _ } = prepare_params(SpParams),
    close_prepared_query(PoolName,io_lib:format("select a.* from ~s(~s) as a",[SpName,Params])).


prepare_params(Parameter) when is_map(Parameter)->
    {K,V,_} = maps:fold(  fun(K,V, {Ka,Va,Pos}) -> {[io_lib:format("~s := $~w",[K, Pos]) |Ka],[V|Va], Pos +1 } end, {[],[],1}, Parameter),
    {ok,string:join( K,","),lists:reverse(V)};
prepare_params(Parameter) when is_list(Parameter)->
    {K,_} =lists:foldl(fun(_K,{Ka,Pos}) -> { [io_lib:format("$~w",[Pos]) |Ka],Pos +1 } end, {[],1},Parameter),
    {ok,string:join(K,","),Parameter}.

update_type_cache(PoolName,DynamicTypes) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {update_type_cache,DynamicTypes}) end).



to_map(Cols, Rows) ->
    [ maps:from_list(lists:zipwith(fun(#column{name = N, type = Type}, V) ->
              case Type of
                  int2 -> {Vi,_} = string:to_integer(V),{N, Vi};
                  int4 -> {Vi,_} = string:to_integer(V),{N, Vi};
                  int8 -> {Vi,_} = string:to_integer(V),{N, Vi};
                  _    -> {N, V}
              end
         end,
        
        Cols, tuple_to_list(Row))) || Row <- Rows ].

handle_x_log_data(StartLSN, EndLSN, Data, CbState) ->
    io:format("~p~n", [{StartLSN, EndLSN, Data}]),
    {ok, EndLSN, EndLSN, CbState}.

separate_keys_values(Map) when is_map(Map) ->
    maps:fold(fun accum_map_keys_values/3,{[],[]},Map);
separate_keys_values(List) when is_list(List) ->
   lists:foldl(fun accum_list_keys_values/2,{[],[]},List).
accum_map_keys_values(Key,Value,{Keys,Values})->
    {[Key|Keys],[Value|Values]}.

accum_list_keys_values({Key,Value},{Keys,Values})->
    {[Key|Keys],[Value|Values]}.

separate_sp_keys_values_option(Sps_ParamValues_Option) ->
    ReversedSps_ParamValues_Option = lists:reverse(Sps_ParamValues_Option),
    separate_sp_keys_values_option(ReversedSps_ParamValues_Option,[]).

separate_sp_keys_values_option([],Accum) ->
    Accum;
separate_sp_keys_values_option([{SpName,ParamValues,Option}|T],Accum) ->
    separate_sp_keys_values_option(T,[{SpName,separate_keys_values(ParamValues),Option}|Accum]).

%postprocessBatchCall({{results,Results},{columns,Columns}}) ->
%   lists:zipwith(fun({Re,Dat} =_D,C) -> {Re,C,Dat} end,Results,Columns);
%postprocessBatchCall(Result) ->
%  error_logger:info_msg("nichtgetroffen ~p",[Result]),
%Result.

postprocessBatchCall({{results,Results},{columns,Columns}}) ->
    postprocessBatchCall(Results,Columns,[]);
postprocessBatchCall(Result) ->
    error_logger:info_msg("nichtgetroffen ~p",[Result]),
    Result.

postprocessBatchCall([],_Columns,Accum) ->
    lists:reverse(Accum);
postprocessBatchCall([{ok = Status,Data} = Result|Results],[Column|Columns],Accum) ->
   postprocessBatchCall(Results,Columns,[postprocess_proc_call({Status,Column,Data}) |Accum]);

postprocessBatchCall([{Status,Data} = Result|Results],[Column|Columns],Accum) ->
   postprocessBatchCall(Results,Columns,[postprocess_proc_call({Status,Data}) |Accum]).





postprocess_proc_call({ok,[#column{name=Name}],[{<<>>}]}) ->
    {ok,Name,[]};
postprocess_proc_call({ok,[#column{name=Name}],[{Element}]}) ->
    {ok,Name,Element};
postprocess_proc_call({ok,[H|_] = Columns,Elements}) when is_record(H,column)-> 
    Names = [X#column.name||X <-Columns],
    {ok,Names,Elements};
postprocess_proc_call({error,#error{code     = <<"P0001">>,
                                    codename = raise_exception,
                                    message  = ErrorCodeBinary ,
                                    extra    = Extra           }})   -> 
    ErrorText = proplists:get_value(detail,Extra,<<"no ErrorText">>),
    StringErrorCode = binary_to_list(ErrorCodeBinary),
    case string:to_integer(StringErrorCode) of
        {error,no_integer} -> {error,9999999  ,<<ErrorCodeBinary/binary, ", ",ErrorText/binary>>};
        {IntErrorCode  ,_} -> {error,IntErrorCode,ErrorText}
    end;

postprocess_proc_call({error,#error{code     = <<"NDREL">>,
                                    message  = ErrorCodeBinary ,
                                    extra    = Extra           }})   -> 
    ErrorText = proplists:get_value(detail,Extra,<<"no ErrorText">>),
    StringErrorCode = binary_to_list(ErrorCodeBinary),
    case string:to_integer(StringErrorCode) of
        {error,no_integer} -> {release, 9999999  ,<<ErrorCodeBinary/binary, ", ",ErrorText/binary>>};
        {IntErrorCode  ,_} -> {release, IntErrorCode,ErrorText}
    end;

postprocess_proc_call({error,#error{code     = SQLState,
                                    codename = undefined_function,
                                    message  = ErrorText ,
                                    extra    = Extra           }})   -> 
    {error,999999,<<SQLState/binary, ", undefined_function, " ,ErrorText/binary>>};

postprocess_proc_call({error,#error{code     = SQLState,
                                    codename = ErrorCondition,
                                    %message  = ErrorCodeBinary ,
                                    extra    = Extra           }})   -> 
    ErrorConditionBinary = atom_to_binary(ErrorCondition,unicode),
    SchemaName           = proplists:get_value(schema_name,Extra,<<"">>),
    ColumnName           = proplists:get_value(column_name,Extra,<<"">>),
    TableName            = proplists:get_value(table_name ,Extra,<<"">>),
    Where                = proplists:get_value(where      ,Extra,<<"">>),
    Position             = lists:last(string:split(Where,<<"\"\n">>)),
    {error,999999,<<SQLState/binary, ", " ,ErrorConditionBinary/binary, ", ", SchemaName/binary,".", TableName/binary,".", ColumnName/binary,", ", Position/binary>>};

postprocess_proc_call(Result) ->
    error_logger:info_msg("nicht getroffen ~p",Result),
    Result.





%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(EUNIT).

separate_keys_values_map_test()->
    ParamsMap = #{ato => ato,"str" => "str",<<"bin">> => <<"bin">>},
    ?assertEqual({[<<"bin">>,"str",ato],[<<"bin">>,"str",ato]},separate_keys_values(ParamsMap)).

separate_keys_values_list_test()->
    ParamsList = [{ato,ato},{"str" ,"str"},{<<"bin">> , <<"bin">>}],
    ?assertEqual({[<<"bin">>,"str",ato],[<<"bin">>,"str",ato]},separate_keys_values(ParamsList)).
separate_sp_keys_values_option_test()->
    Sps_ParamValues_Option = [{sp_call_1,#{p_11 => v_11,p_12 => v_12,p_13=>v_13},opt_1},
                              {sp_call_2,#{p_21 => v_21,p_22 => v_22,p_23=>v_23},opt_2},
                              {sp_call_3,#{p_31 => v_31,p_32 => v_32,p_33=>v_33},opt_3}],
    ?assertEqual([{sp_call_1,{[p_13,p_12,p_11],[v_13,v_12,v_11]},opt_1},
                  {sp_call_2,{[p_23,p_22,p_21],[v_23,v_22,v_21]},opt_2},
                  {sp_call_3,{[p_33,p_32,p_31],[v_33,v_32,v_31]},opt_3}],separate_sp_keys_values_option(Sps_ParamValues_Option)).
postprocess_proc_call_single_row_single_column_nodata_test () ->
    Res = {ok,[{column,<<"result">>,json,-1,-1,1}],[{<<>>}]},
    Expected = {ok,<<"result">>,[]},
    ?assertEqual(Expected,postprocess_proc_call(Res)).
postprocess_proc_call_single_row_single_column_test () ->
    Res = {ok,[{column,<<"result">>,json,-1,-1,1}],[{<<"{\"branch_update_id\":29,\"update_account\":[{\"account_id\":8,\"currency_id\":74,\"balance\":-123.45,\"as_of_date\":\"2018-07-12T00:00:00+00:00\"},{\"account_id\":4,\"currency_id\":74,\"balance\":123.45,\"as_of_date\":\"2018-07-12T00:00:00+00:00\"}]}">>}]},
    Expected = {ok,<<"result">>,<<"{\"branch_update_id\":29,\"update_account\":[{\"account_id\":8,\"currency_id\":74,\"balance\":-123.45,\"as_of_date\":\"2018-07-12T00:00:00+00:00\"},{\"account_id\":4,\"currency_id\":74,\"balance\":123.45,\"as_of_date\":\"2018-07-12T00:00:00+00:00\"}]}">>},
    ?assertEqual(Expected,postprocess_proc_call(Res)).
postprocess_proc_call_multi_column_test() ->
    Res = {ok,[{column,<<"branch_update_id">>,int8,8,-1,1},{column,<<"update_account">>,{unknown_oid,27902},-1,-1,0}],[{29,<<"{\"(8,74,-123.45,\\\"2018-07-12 00:00:00+00\\\")\",\"(4,74,123.45,\\\"2018-07-12 00:00:00+00\\\")\"}">>}]},
    Expected = {ok,[<<"branch_update_id">>,<<"update_account">>],[{29,<<"{\"(8,74,-123.45,\\\"2018-07-12 00:00:00+00\\\")\",\"(4,74,123.45,\\\"2018-07-12 00:00:00+00\\\")\"}">>}]},
    ?assertEqual(Expected,postprocess_proc_call(Res)).
postprocess_proc_call_raise_exception_test() ->
   Res = {error,{error,error,<<"P0001">>,raise_exception,<<"999999">>,[{detail,<<"error_text to be set">>},{file,<<"pl_exec.c">>},{line,<<"3337">>},{routine,<<"exec_stmt_raise">>},{severity,<<"ERROR">>},{where,<<"PL/pgSQL function sp_raise_exception(integer,character varying) line 8 at RAISE\nSQL statement  \"SELECT sp_raise_exception(999999)\"\nPL/pgSQL function sp_branch_confirm_update(bigint,bigint) line 12 at PERFORM">>}]}},
   Expected = {error,999999,<<"error_text to be set">>},
   ?assertEqual(Expected,postprocess_proc_call(Res)).
postprocess_proc_call_raise_exception_noint_test() ->
   Res = {error,{error,error,<<"P0001">>,raise_exception,<<"ABCDEFG">>,[{detail,<<"error_text to be set">>},{file,<<"pl_exec.c">>},{line,<<"3337">>},{routine,<<"exec_stmt_raise">>},{severity,<<"ERROR">>},{where,<<"PL/pgSQL function sp_raise_exception(integer,character varying) line 8 at RAISE\nSQL statement  \"SELECT sp_raise_exception(999999)\"\nPL/pgSQL function sp_branch_confirm_update(bigint,bigint) line 12 at PERFORM">>}]}},
   Expected = {error,9999999,<<"ABCDEFG, error_text to be set">>},
   ?assertEqual(Expected,postprocess_proc_call(Res)).
postprocess_proc_call_dbexception_test() ->
   Res = {error,{error,error,<<"23502">>,not_null_violation,<<"null value in column \"branch_account_id\" violates not-null constraint">>,[{column_name,<<"branch_account_id">>},{detail,<<"Failing row contains (29, null, 2018-07-12 06:11:07.807736+00, 2018-07-12 08:14:20.935462+00).">>},{file,<<"execMain.c">>},{line,<<"2008">>},{routine,<<"ExecConstraints">>},{schema_name,<<"public">>},{severity,<<"ERROR">>},{table_name,<<"branch_update">>},{where,<<"SQL statement \"update branch_update b\n     set datetime_transfer   = clock_timestamp(),\n     branch_account_id = null\n   where b.branch_account_id = v_branch_account_id\n     and b.branch_update_id  = v_branch_update_id\"\nPL/pgSQL function sp_branch_confirm_update(bigint,bigint) line 8 at SQL statement">>}]}},
   Expected = {error,999999,<<"23502, not_null_violation, public.branch_update.branch_account_id, PL/pgSQL function sp_branch_confirm_update(bigint,bigint) line 8 at SQL statement">>}   ,
   ?assertEqual(Expected,postprocess_proc_call(Res)).   
postprocessBatchCall_test() ->
   Res = {{results,[{ok,[{<<>>}]},
                    {ok,[{<<>>}]},
                    {ok,[{7,3}]},
                    {ok,[{<<>>}]},
                    {ok,[{<<>>}]},
                    {ok,[{5,4}]}
                   ]},
           {columns,[[{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"ta_id">>,int8,8,-1,1},{column,<<"transaction_id">>,int8,8,-1,1}],
                     [{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"ta_id">>,int8,8,-1,1},{column,<<"transaction_id">>,int8,8,-1,1}]]}},
    Expected = [{ok,<<"a">>,[]},
                {ok,<<"a">>,[]},
                {ok,[<<"ta_id">>,<<"transaction_id">>],[{7,3}]},
                {ok,<<"a">>,[]},
                {ok,<<"a">>,[]},
                {ok,[<<"ta_id">>,<<"transaction_id">>],[{5,4}]}
               ],
    ?assertEqual(Expected,postprocessBatchCall(Res)).

postprocessBatchCall_error_test() ->
   Res = {{results,[{ok,[{<<>>}]},
                    {error,{error,error,<<"42883">>,undefined_function,<<"function sp_save_branch_checkout_line_item(unit => character varying, tax_rate => numeric, tax_amount => numeric, quantity => numeric, product_code => character varying, gross_amount => numeric, branch_checkout_line_item_id => integer, branch_checkout_header_id => bigint, branch_account_id => bigint, article_name => character varying) does not exist">>,[{file,<<"parse_func.c">>},{hint,<<"No function matches the given name and argument types. You might need to add explicit type casts.">>},{line,<<"528">>},{position,<<"17">>},{routine,<<"ParseFuncOrColumn">>},{severity,<<"ERROR">>}]}}]},
           {columns,[[{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"ta_id">>,int8,8,-1,1},{column,<<"transaction_id">>,int8,8,-1,1}],
                     [{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"a">>,void,4,-1,0}],[{column,<<"a">>,void,4,-1,0}],
                     [{column,<<"ta_id">>,int8,8,-1,1},{column,<<"transaction_id">>,int8,8,-1,1}]]}},
    Expected = [{ok,<<"a">>,[]},
                {error,999999,<<"42883, undefined_function, function sp_save_branch_checkout_line_item(unit => character varying, tax_rate => numeric, tax_amount => numeric, quantity => numeric, product_code => character varying, gross_amount => numeric, branch_checkout_line_item_id => integer, branch_checkout_header_id => bigint, branch_account_id => bigint, article_name => character varying) does not exist">>}
               ],
    ?assertEqual(Expected,postprocessBatchCall(Res)).
-endif.



   



