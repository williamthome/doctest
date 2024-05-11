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
-module(doctest_eunit).
-moduledoc false.

% API functions
-export([test/1]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

test(Tests) ->
    eunit:test(Tests, options()).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

options() ->
    case erlang:module_loaded(rebar3) of
        true ->
            resolve_eunit_opts(rebar3:init_config());
        false ->
            []
    end.

%%%=====================================================================
%%% rebar3 non-exported functions
%%%=====================================================================

% TODO: Maybe submit a PR exporting 'resolve_eunit_opts/1'.
% @see https://github.com/erlang/rebar3/blob/b64d94f4e6fb738c4a3004faf833e0b9617d86a8/apps/rebar/src/rebar_prv_eunit.erl#L443

resolve_eunit_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    EUnitOpts = rebar_state:get(State, eunit_opts, []),
    EUnitOpts1 = case proplists:get_value(verbose, Opts, false) of
                    true  -> set_verbose(EUnitOpts);
                    false -> EUnitOpts
                 end,
    EUnitOpts2 = case proplists:get_value(profile, Opts, false) of
                    true  -> set_profile(EUnitOpts1);
                    false -> EUnitOpts1
                 end,
    IsVerbose = lists:member(verbose, EUnitOpts2),
    case proplists:get_value(eunit_formatters, Opts, not IsVerbose) of
        true  -> custom_eunit_formatters(EUnitOpts2);
        false -> EUnitOpts2
    end.

custom_eunit_formatters(Opts) ->
    ReportOpts = custom_eunit_report_options(Opts),
    %% If `report` is already set then treat that like `eunit_formatters` is false
    case lists:keymember(report, 1, Opts) of
        true -> Opts;
        false -> [no_tty, {report, {eunit_progress, ReportOpts}} | Opts]
    end.

custom_eunit_report_options(Opts) ->
    case lists:member(profile, Opts) of
        true -> [colored, profile];
        false -> [colored]
    end.

set_profile(Opts) ->
    %% if `profile` is already set don't set it again
    case lists:member(profile, Opts) of
        true  -> Opts;
        false -> [profile] ++ Opts
    end.

set_verbose(Opts) ->
    %% if `verbose` is already set don't set it again
    case lists:member(verbose, Opts) of
        true  -> Opts;
        false -> [verbose] ++ Opts
    end.
