%%% ---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @doc Provides functions to test docs.
%%% @end
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

% API functions
-export([module/1, module/2, forms/2]).

-export_type([options/0, result/0]).

-type options() :: #{
    enabled => boolean(),
    moduledoc => boolean(),
    doc => boolean() | [{atom(), arity()}],
    eunit_opts => rebar3_config | [term()],
    extractors => [module()]
}.
-type result() :: ok | error.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-spec module(Mod) -> Result when
      Mod :: module(),
      Result :: result().

module(Mod) ->
    module(Mod, #{}).

-spec module(Mod, Opts) -> Result when
      Mod :: module(),
      Opts :: options(),
      Result :: result().

module(Mod, Opts) ->
    run(module_tests, Mod, Opts).

-spec forms(Forms, Opts) -> Result when
      Forms :: [erl_syntax:syntaxTree()],
      Opts :: options(),
      Result :: result().

forms(Forms, Opts) ->
    run(forms_tests, Forms, Opts).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

run(Fun, Payload, Opts) ->
    do_run(parse_opts(Opts), Fun, Payload).

do_run(#{enabled := true, eunit_opts := EunitOpts} = Opts, Fun, Payload) ->
    doctest_eunit:test(doctest_extract:Fun(Payload, Opts), EunitOpts);
do_run(#{enabled := false}, _, _) ->
    ok.

parse_opts(Opts) when is_map(Opts) ->
    #{
        enabled => maps:get(enabled, Opts,
            application:get_env(doctest, enabled, true)),
        moduledoc => maps:get(moduledoc, Opts,
            application:get_env(doctest, moduledoc, true)),
        doc => maps:get(doc, Opts,
            application:get_env(doctest, doc, true)),
        eunit_opts => maps:get(eunit_opts, Opts,
            application:get_env(doctest, eunit_opts, rebar3_config)),
        extractors => maps:get(extractors, Opts,
            application:get_env(doctest, extractors, []))
    }.
