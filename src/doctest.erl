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

-export([compile/3]).

-type options() :: #{
    moduledoc => boolean(),
    funs => boolean() | [{atom(), arity()}],
    eunit => resolve | [term()]
    % TODO: extractors => [module()]
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

compile(Mod, Extractors, Opts) ->
    Forms = module_forms(Mod),
    CompileInfo = Mod:module_info(compile),
    {ok, Mod, Bin} = compile:forms(Forms, [
        binary,
        debug_info,
        % export_all,
        % nowarn_export_all,
        {i, "eunit/include/eunit.hrl"}
        % | CompileInfo
    ]),
    {source, Filename} = proplists:lookup(source, CompileInfo),
    % NOTE: Load module is needed for parse_transformation.
    %       Check if spawn solves this issue by waiting for it.
    % Maybe create a temp mod and export all functions to enable internal funs:
    % 1> TmpMod = <unique_name>,
    % 2> {module, TmpMod} = code:load_binary(TmpMod, Filename, Bin).
    % And after:
    % 3> code:purge(TmpMod), code:delete(TmpMod).
    doctest_eunit:test({test_desc(Mod),
        all_test_cases(Extractors, {Mod, Bin, Filename, Forms}, Opts)}).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

module_forms(Mod) ->
    {ok, {Mod, [{abstract_code, {_, AST}}]}} =
        beam_lib:chunks(code:which(Mod), [abstract_code]),
    AST.

test_desc(Mod) ->
    iolist_to_binary(io_lib:format("module '~w'", [Mod])).

all_test_cases(Extractors, Args, Opts) ->
    sort_test_cases(lists:flatten(lists:map(fun(Extractor) ->
        test_cases(Extractor, Extractor:chunks(Args), Opts)
    end, Extractors))).

test_cases(Extractor, Chunks, Opts) ->
    lists:filtermap(fun({Kind, Ln, Doc}) ->
        case {Extractor:code_blocks(Doc), Kind} of
            {{ok, CodeBlocks}, {doc, {M, F, A}}} ->
                case should_test_doc(Opts, {F, A}) of
                    true ->
                        {true, {Ln, doctest_eunit:doc_tests({M, F, A}, Ln, CodeBlocks)}};
                    false ->
                        false
                end;
            {{ok, CodeBlocks}, {moduledoc, M}} ->
                case should_test_moduledoc(Opts) of
                    true ->
                        {true, {Ln, doctest_eunit:moduledoc_tests(M, Ln, CodeBlocks)}};
                    false ->
                        false
                end;
            {none, _} ->
                false
        end
    end, Chunks).

sort_test_cases(TestCases) ->
    lists:map(fun({_Ln, TestCase}) ->
        TestCase
    end, lists:keysort(1, TestCases)).

should_test_moduledoc(#{moduledoc := true}) ->
    true;
should_test_moduledoc(#{moduledoc := false}) ->
    false;
should_test_moduledoc(Opts) when not is_map_key(moduledoc, Opts) ->
    true.

should_test_doc(#{funs := true}, _Fun) ->
    true;
should_test_doc(#{funs := false}, _Fun) ->
    false;
should_test_doc(#{funs := Funs}, Fun) when is_list(Funs) ->
    lists:member(Fun, Funs);
should_test_doc(Opts, _Fun) when not is_map_key(funs, Opts) ->
    true.
