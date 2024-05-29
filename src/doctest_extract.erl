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
-module(doctest_extract).
-moduledoc false.

% API functions
-export([module_tests/3, forms_tests/3, code_blocks/2, default_extractors/0]).

% TODO: Fix callbacks
% -callback extract_module_tests(Mod, ShouldTestModDoc, FunsOpts) -> Result when
%           Mod :: module(),
%           ShouldTestModDoc :: boolean(),
%           FunsOpts :: boolean() | [{FunName, FunArity}],
%           FunName :: atom(),
%           FunArity :: arity(),
%           Result :: {ok, Tests} | {error, term()},
%           Tests :: list().

-callback code_blocks(binary()) -> code_blocks().

-type code_blocks() :: [{binary(), location()}] | none.
-type location() :: {Ln :: pos_integer(), Col :: pos_integer()}.

-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE < 27).
        -define(MARKDOWN_SUPPORTED, false).
    -else.
        -define(MARKDOWN_SUPPORTED, true).
    -endif.
-else.
    -define(MARKDOWN_SUPPORTED, false).
-endif.

%%%=====================================================================
%%% API functions
%%%=====================================================================

module_tests(Mod, Extractors, Opts) when is_atom(Mod), is_list(Extractors) ->
    forms_tests(module_forms(Mod), Extractors, Opts).

forms_tests(Forms, Extractors, Opts) when is_list(Forms), is_list(Extractors) ->
    {ok, Mod, Bin} = compile:forms(Forms, [
        binary,
        debug_info,
        {i, "eunit/include/eunit.hrl"}
    ]),
    {source, Filename} = proplists:lookup(source, Mod:module_info(compile)),
    ExtractorArgs = {Mod, Bin, Filename, Forms},
    {test_desc(Mod), all_test_cases(Extractors, ExtractorArgs, Opts)}.

code_blocks(Doc, RE) when is_binary(Doc) ->
    case re:run(Doc, RE, [global, {capture, all_but_first, index}]) of
        {match, Groups} ->
            {ok, [{binary_part(Doc, Pos, Len), loc(Doc, Pos)}
                 || [_, {Pos, Len}, _, _] <- Groups]};
        nomatch ->
            none
    end.

-if(?MARKDOWN_SUPPORTED).
default_extractors() ->
    [doctest_markdown].
-else.
default_extractors() ->
    [doctest_comment].
-endif.

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

loc(Doc, Pos) ->
    Pre = binary_part(Doc, 0, Pos),
    parse_loc(Pre, {1, 1}).

parse_loc(<<$\r, $\n, Rest/binary>>, {Ln, _Col}) ->
    parse_loc(Rest, {Ln+1, 1});
parse_loc(<<$\r, Rest/binary>>, {Ln, _Col}) ->
    parse_loc(Rest, {Ln+1, 1});
parse_loc(<<$\n, Rest/binary>>, {Ln, _Col}) ->
    parse_loc(Rest, {Ln+1, 1});
parse_loc(<<_, Rest/binary>>, {Ln, Col}) ->
    parse_loc(Rest, {Ln, Col+1});
parse_loc(<<>>, {Ln, Col}) ->
    {Ln, Col}.
