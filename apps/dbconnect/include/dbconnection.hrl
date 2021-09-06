-type sp_param_values_option() :: {spname(),{paramlist(),list()},spoption()}.
-type batch()::list({epgsql:statement(),list()}).
-type spname()::atom()|string()|bitstring().
-type paramtype()::atom()|string()|bitstring().
-type paramlist():: list(paramtype()).
-type spoption():: plain|json|json_agg|json_strip_nulls|jsonagg_strip_nulls.

