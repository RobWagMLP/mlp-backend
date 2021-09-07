%%%-------------------------------------------------------------------
%%% @copyright 2009 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Code generation.
%%% @end
%%%-------------------------------------------------------------------
-module(abnfc_gen).

%% API
-export([generate/2]).

-compile(export_all).

-import(erl_syntax, [application/2,
		     arity_qualifier/2,
		     atom/1,
		     attribute/2,
		     binary/1,
		     binary_field/1,
		     binary_field/2,
		     case_expr/2,
		     class_qualifier/2,
		     clause/3,
		     comment/1,
		     conjunction/1,
		     eof_marker/0,
		     form_list/1,
		     fun_expr/1,
		     function/2,
		     infix_expr/3,
		     integer/1,
		     list/1,list/2,
		     match_expr/2,
		     nil/0,
		     operator/1,
		     string/1,
		     try_expr/3,
		     tuple/1,
		     variable/1
		     ]).

-include("abnfc_ast.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
generate(AST, Opts) ->
    Verbose = proplists:get_bool(verbose,Opts),
    maybe_write("Opts ~p~n",[Opts],Verbose),
    Module = proplists:get_value(mod,Opts),
    Type = case proplists:get_bool(binary, Opts) of
	       true -> binary;
	       false -> list
	   end,
    Names = [R#rule.name||R<-AST],
    Res=form_list([header_comments(),
		   attribute(atom(module), [atom(Module)]),
		   exports(Names),
		   attribute(atom(include),[string(lists:concat([Module,".hrl"]))]),
		   gen_dec(Names),
		   form_list([form_list([gen_rule(R, Type, Verbose)]) || R <- AST]),
		   mk__alt(),
		   mk__repeat(),
		   mk__seq(),
		   eof_marker()]),
    {ok, erl_prettypr:format(Res)}.

%%====================================================================
%% Internal functions
%%====================================================================
header_comments() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    DT = lists:flatten(
    	   io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
    			 [Year,Month,Day,Hour,Min,Sec])),
    comment(["%% Do not modify this file, it is automatically generated by abnfc.",
	     "%% All changes will be lost when it is regenerated.",
	     "%% Generated by abnfc_gen on "++DT]).

exports(Funs) ->
    attribute(atom(export),
	      [list([arity_qualifier(atom(decode),integer(2))]++
		    [arity_qualifier(atom(F),integer(0))||F<-Funs])]).

gen_dec(Names) ->
    function(atom(decode),
	     [clause([atom(Name),variable('Str')],[],
		     [application(application(atom(Name),[]),[variable('Str')])])||Name<-Names]).

gen_rule(#rule{name=Name, body=Element, code=nocode}, Type, Verbose) ->
    maybe_write("abnfc_gen: generating rule ~p~n",[Name],Verbose),
    Body = gen_elem(Element, Type),
    mk_rule_fun(Name, Body);

gen_rule(#rule{name=Name, body=Element, code=Code}, Type, Verbose) ->
    maybe_write("abnfc_gen: generating rule ~p~n",[Name],Verbose),
    Vars = gen_vars(Element),
    Body = gen_elem(Element, Type),
    mk_rule_fun(Name, Body, Vars, Code).

mk_rule_fun(Name, Body) ->
    function(
      atom(Name),
      [clause([],[],
	      [fun_expr([clause([variable('T')],
	      			[],
				[match_expr(variable('__P'),Body),
				 application(variable('__P'),
					     [variable('T')])])])])]).

mk_rule_fun(Name, Body, Vars, Code) ->
    function(
      atom(Name),
      [clause([],[],
	      [fun_expr([clause([variable('T')],
	      			[],
				[match_expr(variable('__P'),Body),
				 case_expr(
				   application(variable('__P'),[variable('T')]),
				   [clause([tuple([atom(ok),Vars,variable('_T1')])],
					   [],
					   [try_expr(Code,
						     [clause([variable('__Ret')],[],
							     [tuple([atom(ok), variable('__Ret'),
								     variable('_T1')])])],
						     %% Handlers
						     [clause([class_qualifier(atom(throw),
									      atom(fail))],
							     [],
							     [atom(fail)])
						     ]
						    )]
					  ),
				    clause([atom(fail)],[],[atom(fail)])])])])])]).

gen_elem(#seq{elements=Elements}, Type) ->
    Body = [gen_elem(Element, Type)||Element <- Elements],
    application(atom('__seq'),[list(Body)]);

gen_elem(#alt{alts=Elements}, Type) ->
    Alts = [gen_elem(Element, Type)||Element <- Elements],
    application(atom('__alt'),[list(Alts)]);

gen_elem(#repeat{min=Min, max=infinity, body=Elem}, Type) ->
    application(atom('__repeat'),
		[integer(Min), atom(infinity), gen_elem(Elem, Type)]);
gen_elem(#repeat{min=Min, max=Max, body=Elem}, Type) ->
    application(atom('__repeat'),
		[integer(Min), integer(Max), gen_elem(Elem, Type)]);

gen_elem(#char_val{value=Num}, Type) ->
    PList=mk_plist(Num,Type),
    fun_expr(
      [clause(PList,
	      [],
	      [tuple([atom(ok), integer(Num), variable('Tl')])]),
       clause([variable('_')], [], [atom(fail)])]);


gen_elem(#char_range{from=From, to=To}, Type) ->
    PList=mk_plist('C',Type),
    fun_expr(
      [clause(
	 PList,
	 [range_test(variable('C'), From, To)],
	 [tuple([atom(ok), variable('C'), variable('Tl')])]),
       clause([variable('_')], [], [atom(fail)])]);

gen_elem(#char_alt{alts=Alts}, Type) ->
    PList=mk_plist('C',Type),
    fun_expr([clause(PList,[char_guard(variable('C'),A)],
		     [tuple([atom(ok),variable('C'),variable('Tl')])])||A<-Alts]
	     ++[clause([variable('_')],[],[atom(fail)])]);

gen_elem(#char_seq{elements=Nums}, Type) ->
    ParList = num_par_list(length(Nums), Type),
    Guard = num_guard(Nums),
    Result=[variable(list_to_atom(lists:concat(["C",N]))) || N <- lists:seq(1,length(Nums))],
    fun_expr([clause([ParList],Guard,
		     [tuple([atom(ok), list(Result), variable('Tl')])]),
	      clause([variable('_')],[],[atom(fail)])]);

gen_elem(#rulename{name=Rule}, _Type) when is_atom(Rule) ->
    application(atom(Rule),[]).

mk_plist(Char, binary) when is_integer(Char) ->
    [binary([binary_field(integer(Char)),binary_field(variable('Tl'),[atom('binary')])])];
mk_plist(Var, binary) when is_atom(Var) ->
    [binary([binary_field(variable(Var)),binary_field(variable('Tl'),[atom('binary')])])];
mk_plist(Char, list) when is_integer(Char) ->
    [list([integer(Char)],variable('Tl'))];
mk_plist(Var, list) ->
    [list([variable(Var)],variable('Tl'))].

gen_vars(#seq{elements=Es}) ->
    Vs = list([variable(list_to_atom(lists:concat(["_YY",N])))
	       ||N<-lists:seq(1,length(Es))]),
    match_expr(Vs,variable('_YY'));
gen_vars(_) ->
    variable('_YY').

num_guard(Cs) ->
    {_,R} = lists:foldl(
	      fun (#char_range{from=From,to=To},{Pos,Acc}) ->
		      Var=variable(list_to_atom(lists:concat(["C",Pos]))),
		      {Pos+1, [range_test(Var, From, To)|Acc]};
		  (#char_val{value=C}, {Pos,Acc}) ->
		      Var=variable(list_to_atom(lists:concat(["C",Pos]))),
		      {Pos+1, [val_test(Var, C)|Acc]};
		  (#char_alt{alts=Alts}, {Pos,Acc}) ->
		      {Pos+1,[num_alt_guard(Pos,Alts)|Acc]}
	      end, {1,[]}, Cs),
    conjunction(lists:reverse(R)).

num_par_list(Len, binary) ->
    binary([binary_field(variable(list_to_atom("C"++integer_to_list(N))))||
	       N <- lists:seq(1,Len)]++
	   [binary_field(variable('Tl'),[atom('binary')])]);
num_par_list(Len, list) ->
    list([variable(list_to_atom("C"++integer_to_list(N))) ||
    	     N <- lists:seq(1,Len)],variable('Tl')).


num_alt_guard(Var, Alts) ->
    R = lists:foldl(
	  fun (#char_range{from=From,to=To},Acc) ->
		  VarT=variable(list_to_atom(lists:concat(["C",Var]))),
		  [range_test(VarT, From, To) |Acc];
	      (#char_val{value=C}, Acc) ->
		  VarT=variable(list_to_atom(lists:concat(["C",Var]))),
		  [val_test(VarT, C)|Acc]
	  end, [], Alts),
    disjunction(lists:reverse(R)).


char_guard(Var,#char_val{value=Val}) ->
    val_test(Var, Val);
char_guard(Var, #char_range{from=From,to=To}) ->
    range_test(Var, From, To).

range_test(Var, From, To) ->
    infix_expr(infix_expr(Var,operator('>='),integer(From)),
	       operator('and'),
	       infix_expr(Var,operator('=<'),integer(To))).
val_test(Var, Val) ->
    infix_expr(Var, operator('=='), integer(Val)).

disjunction([H|T]) when T/=[] ->
    infix_expr(H,operator('or'),disjunction(T));
disjunction([As]) ->
    As.

%%================================================
%% Run-time support functions
%%================================================
%% Match one of several parsers."

mk__alt() ->
    function(atom('__alt'),
	     [clause(
		[list([variable('P')],variable('Ps'))],
		[],
		[fun_expr(
		   [clause(
		      [variable('T')],
		      [],
		      [case_expr(
			 application(variable('P'),[variable('T')]),
			 [clause([match_expr(tuple([atom(ok),variable('_R'),variable('_T1')]),
					     variable('Res'))],[],
				 [variable('Res')]),
			  clause([atom(fail)],[],
				 [case_expr(
				    variable('Ps'),
				    [clause([nil()], [], [atom(fail)]),
				     clause([variable('_')],[],
					    [application(
					       application(atom('__alt'), [variable('Ps')]),
					       [variable('T')])
					    ]
					   )]
				   )]
				)]
			)]
		     )]
		  )]
	       )]
	    ).


%% Match between Min and Max repetitions of parser P
mk__repeat() ->
    form_list([mk__repeat_3(),
	       mk__repeat_4()]).

mk__repeat_3() ->
    function(atom('__repeat'),
	     [clause(
		[variable('Min'),variable('Max'),variable('P')],
		[],
		[application(atom('__repeat'),
			     [variable('Min'),
			      variable('Max'),
			      variable('P'),
			      integer(0)])])]).

mk__repeat_4() ->
    function(atom('__repeat'),
	     [clause(
		[variable('Min'),variable('Max'),variable('P'), variable('Found')],
		[],
		[fun_expr(
		   [clause(
		      [variable('T')],
		      [],
		      [case_expr(
			 application(variable('P'),[variable('T')]),
			 [clause([tuple([atom(ok),variable('R1'),variable('T1')])],
				 [infix_expr(variable('Max'),operator('=='),
					     infix_expr(variable('Found'),operator('+'),
							integer(1)))],
				 [tuple([atom(ok),list([variable('R1')]),variable('T1')])]),
			  clause([tuple([atom(ok),variable('R1'),variable('T1')])],
				 [],
				 [
				  case_expr(application(
					      application(atom('__repeat'),
							  [variable('Min'),
							   variable('Max'),
							   variable('P'),
							   infix_expr(variable('Found'),
								      operator('+'),
								      integer(1))]),
					      [variable('T1')]),
					    [clause([tuple([atom(ok),variable('R2'),
							    variable('T2')])],
						    [],
						    [tuple([atom(ok),
							    list([variable('R1')],variable('R2')),
							    variable('T2')])]),
					     clause([atom(fail)],
						    [infix_expr(variable('Found'),
								operator('>='),
								variable('Min'))],
						    [tuple([atom(ok),list([variable('R1')]),
							    variable('T1')])]),
					     clause([atom(fail)],
						    [],[atom(fail)])
					    ])]),
			  clause([atom(fail)],
				 [infix_expr(variable('Found'),operator('>='),variable('Min'))],
				 [tuple([atom(ok),nil(),variable('T')])]),
			  clause([atom(fail)],
				 [],[atom(fail)])
			 ])])])])]).

%% Match a sequence of parsers.
mk__seq() ->
    function(atom('__seq'),
	     [clause(
		[list([variable('P')],variable('Ps'))],
		[],
		[fun_expr(
		   [clause(
		      [variable('T')],
		      [],
		      [case_expr(
			 application(variable('P'),[variable('T')]),
			 [clause([tuple([atom(ok),variable('R1'),variable('T1')])],
				 [],
				 [
				  case_expr(
				    application(
				      application(atom('__seq'),[variable('Ps')]),
				      [variable('T1')]),
				    [clause([tuple([atom(ok),variable('R2'),variable('T2')])],
					    [],
					    [tuple([atom(ok),list([variable('R1')],
								  variable('R2')),
						    variable('T2')])]),
				     clause([atom(fail)],[],[atom(fail)])])]),
			  clause([atom(fail)],[],[atom(fail)])])])])]),
	      clause([nil()],[],
		     [fun_expr(
			[clause([variable('T')],[],
				[tuple([atom(ok),list([]),variable('T')])])])
		     ])]).

maybe_write(Fmt,Args,true) ->
    io:format(Fmt,Args);
maybe_write(_Fmt,_Args,false) ->
    ok.