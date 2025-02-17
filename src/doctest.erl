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
-export([module/1, module/2]).

-export_type([options/0, result/0]).

-type options() :: #{
    moduledoc => boolean(),
    doc => boolean() | [{atom(), arity()}],
    eunit_opts => rebar3_config | [term()],
    extractors => [module()],
    bindings => erl_eval:binding_struct()
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
module(Mod, Opts0) when is_atom(Mod), is_map(Opts0) ->
    Opts = parse_opts(Opts0),
    Tests = doctest_extract:module_tests(Mod, Opts),
    EUnitOpts = maps:get(eunit_opts, Opts),
    doctest_eunit:test(Tests, EUnitOpts).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

parse_opts(Opts) ->
    #{
        moduledoc => maps:get(moduledoc, Opts, true),
        doc => maps:get(doc, Opts, true),
        eunit_opts => maps:get(eunit_opts, Opts, rebar3_config),
        extractors => maps:get(extractors, Opts, default_extractors()),
        bindings => maps:get(bindings, Opts, #{})
    }.

-if(?OTP_RELEASE >= 27).
default_extractors() ->
    [doctest_extract_attr].
-else.
default_extractors() ->
    [doctest_extract_tag].
-endif.
