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
-export([module/1, module/2]).

% Check OTP version >= 27.
-include("doctest_otp_check.hrl").

-type options() :: #{
    moduledoc => boolean(),
    funs => boolean() | [{atom(), arity()}],
    eunit => resolve | [term()]
}.
-type test_result() :: ok | error | {error, term()}.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-doc #{ equiv => module(Mod, #{}) }.
-spec module(module()) -> test_result().

module(Mod) ->
    module(Mod, #{}).

-spec module(module(), options()) -> test_result().

module(Mod, Opts) when is_atom(Mod), is_map(Opts) ->
    ShouldTestModDoc = maps:get(moduledoc, Opts, true),
    FunsOpts = maps:get(funs, Opts, true),
    case doctest_parse:module_tests(Mod, ShouldTestModDoc, FunsOpts) of
        {ok, Tests} ->
            EunitOpts = maps:get(eunit, Opts, resolve),
            doctest_eunit:test(Tests, EunitOpts);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
