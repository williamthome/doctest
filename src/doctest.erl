%%%---------------------------------------------------------------------
%%% Copyright 2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%---------------------------------------------------------------------
-module(doctest).
-moduledoc """
Provides `module/1` and `module/2` to test doc attributes.
""".
-moduledoc #{ author => "William Fank Thomé [https://github.com/williamthome]" }.

% API functions
-export([module/1, module/2, forms/2]).

-type options() :: #{
    enabled => boolean(),
    moduledoc => boolean(),
    funs => boolean() | [{atom(), arity()}],
    eunit => resolve | [term()],
    extractors => [module()]
}.
-type test_result() :: ok | error.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-doc #{ equiv => module(Mod, #{}) }.
-spec module(Mod) -> Result when
      Mod :: module(),
      Result :: test_result().

module(Mod) ->
    module(Mod, #{}).

-spec module(Mod, Opts) -> Result when
      Mod :: module(),
      Opts :: options(),
      Result :: test_result().

module(Mod, Opts) ->
    run(module_tests, Mod, Opts).

-spec forms(Forms, Opts) -> Result when
      Forms :: [erl_syntax:syntaxTree()],
      Opts :: options(),
      Result :: test_result().

forms(Forms, Opts) ->
    run(forms_tests, Forms, Opts).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

run(Fun, Payload, Opts) ->
    do_run(parse_opts(Opts), Fun, Payload).

do_run(#{enabled := true} = Opts, Fun, Payload) ->
    doctest_eunit:test(
        doctest_extract:Fun(Payload, maps:get(extractors, Opts), Opts),
        maps:get(eunit, Opts)
    );
do_run(#{enabled := false}, _, _) ->
    ok.

parse_opts(Opts) when is_map(Opts) ->
    Env = application:get_all_env(doctest),
    #{
        enabled => maps:get(enabled, Opts,
            proplists:get_value(enabled, Env, true)),
        moduledoc => maps:get(moduledoc, Opts,
            proplists:get_value(moduledoc, Env, true)),
        funs => maps:get(funs, Opts,
            proplists:get_value(funs, Env, true)),
        eunit => maps:get(eunit, Opts,
            proplists:get_value(eunit, Env, resolve)),
        extractors => maps:get(extractors, Opts,
            proplists:get_value(extractors, Env, doctest_extract:default_extractors()))
    }.
