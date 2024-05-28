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
-export([module/1, module/2, module/3]).

-type options() :: #{
    moduledoc => boolean(),
    funs => boolean() | [{atom(), arity()}],
    eunit => resolve | [term()],
    % TODO: (do only available for OTP >= 27)
    comments => boolean()
}.
-type test_result() :: ok | error | {error, term()}.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-doc #{ equiv => module(Mod, #{}) }.
-spec module(Mod) -> Result when
      Mod :: module(),
      Result :: test_result().

module(Mod) ->
    module(Mod, #{}).

-doc #{ equiv => module(doctest_extract:default_extractor(), Mod, Opts) }.
-spec module(Mod, Opts) -> Result when
      Mod :: module(),
      Opts :: options(),
      Result :: test_result().

module(Mod, Opts) ->
    module(doctest_extract:default_extractor(), Mod, Opts).

-spec module(Extractor, Mod, Opts) -> Result when
      Extractor :: module(),
      Mod :: module(),
      Opts :: options(),
      Result :: test_result().

module(Extractor, Mod, Opts) when is_atom(Mod), is_map(Opts) ->
    Env = application:get_all_env(doctest),
    ShouldTestModDoc = maps:get(moduledoc, Opts, proplists:get_value(moduledoc, Env, true)),
    FunsOpts = maps:get(funs, Opts, proplists:get_value(funs, Env, true)),
    case doctest_extract:extract_module_tests(Extractor, Mod, ShouldTestModDoc, FunsOpts) of
        {ok, Tests} ->
            EunitOpts = maps:get(eunit, Opts, proplists:get_value(eunit, Env, resolve)),
            doctest_eunit:test(Tests, EunitOpts);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
