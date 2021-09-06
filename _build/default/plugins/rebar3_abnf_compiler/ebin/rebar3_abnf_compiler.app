{application,rebar3_abnf_compiler,
             [{registered,[]},
              {description,"A ABNF compiler for rebar3 3.7+"},
              {vsn,"0.1.2"},
              {applications,[kernel,stdlib,abnfc]},
              {licenses,["Apache 2.0"]},
              {links,[{"Github",
                       "https://github.com/rbkmoney/rebar3_abnf_compiler.git"}]},
              {modules,[rebar3_abnf_compiler,rebar3_abnf_compiler_prv]}]}.
