-module(dbconnect).

-behaviour(application).
-behaviour(supervisor).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("dbconnection.hrl").
-export([]).
-export([start/0, stop/0,start/2, stop/1,start_link/0]).
-export([init/1]).
-export([close_procs/1]).
-export([call_procedure/4]).

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
    QueryString = 
    case AsJson of
      false                 -> io_lib:format("select a.* from ~s(~s) as a",[SpName,Params]);
      true                  -> io_lib:format("select to_json(a) as result from ~s(~s) as a",[SpName,Params]);
      jsonagg               -> io_lib:format("select coalesce(json_agg(a),'[]') as result from ~s(~s) as a",[SpName,Params]);
      [true,strip_nulls]    -> io_lib:format("select coalesce(json_strip_nulls(to_json(a)),'{}') as result from ~s(~s) as a",[SpName,Params]);
      [jsonagg,strip_nulls] -> io_lib:format("select coalesce(json_strip_nulls(json_agg(a)),'[]') as result from ~s(~s) as a",[SpName,Params])
    end,
    prepared_query(PoolName,QueryString,Values).

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


prepare_params(Parameter) when is_map(Parameter)->
    {K,V,_} = maps:fold(  fun(K,V, {Ka,Va,Pos}) -> {[io_lib:format("~s := $~w",[K, Pos]) |Ka],[V|Va], Pos +1 } end, {[],[],1}, Parameter),
    {ok,string:join( K,","),lists:reverse(V)};
prepare_params(Parameter) when is_list(Parameter)->
    {K,_} =lists:foldl(fun(_K,{Ka,Pos}) -> { [io_lib:format("$~w",[Pos]) |Ka],Pos +1 } end, {[],1},Parameter),
    {ok,string:join(K,","),Parameter}.


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