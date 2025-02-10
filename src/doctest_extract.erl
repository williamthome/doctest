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

% Ignores the false positive:
% > The pattern {'file', Filename} can never match the type 'false'
-dialyzer({nowarn_function, module_forms/1}).

% API functions
-export([ module_tests/2
        , module_forms/1
        , forms_tests/2
        , code_blocks/2
        , default_extractors/0
        ]).

-export_type([ doc_token/0
             , moduledoc_token/0
             , token/0
             , chunk/0
             , code_blocks/0
             , location/0
             ]).

-callback chunks(Args) -> Chunks when
          Args :: {Mod, Forms},
          Mod :: module(),
          Forms :: [erl_syntax:syntaxTree()],
          Chunks :: [chunk()].

-callback code_blocks(Doc) -> CodeBlocks when
          Doc :: binary(),
          CodeBlocks :: code_blocks().

-type doc_token() :: {doc, mfa(), Tag :: binary()}.
-type moduledoc_token() :: {moduledoc, module(), Tag :: binary()}.
-type token() :: doc_token() | moduledoc_token().
-type chunk() :: {token(), Ln :: pos_integer(), Doc :: binary()}.
-type code_blocks() :: [{binary(), location()}] | none.
-type location() :: {Ln :: pos_integer(), Col :: pos_integer()}.

-include("doctest_check.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

module_tests(Mod, Opts) when is_atom(Mod) ->
    forms_tests(module_forms(Mod), Opts).

module_forms(Mod) ->
    case is_cover_mod_loaded() andalso cover:is_compiled(Mod) of
        {file, Filename} ->
            do_module_forms(Filename);
        false ->
            do_module_forms(code:which(Mod))
    end.

forms_tests(Forms, Opts) when is_map(Opts) ->
    Mod = doctest_forms:module(Forms),
    Extractors = extractors(Opts),
    {test_desc(Mod), all_test_cases(Extractors, {Mod, Forms}, Opts#{
        extractors => Extractors
    })}.

code_blocks(Doc, RE) when is_binary(Doc) ->
    case re:run(Doc, RE, [global, {capture, all_but_first, index}]) of
        {match, Groups} ->
            {ok, [{binary_part(Doc, Pos, Len), loc(Doc, Pos)}
                 || [_, {Pos, Len}, _, _] <- Groups]};
        nomatch ->
            none
    end.

-if(?IS_DOC_ATTRS_SUPPORTED).
default_extractors() ->
    [doctest_extract_attr, doctest_extract_tag].
-else.
default_extractors() ->
    [doctest_extract_tag].
-endif.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

is_cover_mod_loaded() ->
    Apps = application:loaded_applications(),
    lists:member(fun({Mod, _Desc, _Vsn}) ->
        Mod =:= cover
    end, Apps).

do_module_forms(Filename) ->
    {ok, {_Mod, [{abstract_code, {_, AST}}]}} =
        beam_lib:chunks(Filename, [abstract_code]),
    AST.

extractors(#{extractors := []}) ->
    default_extractors();
extractors(#{extractors := Extractors}) when is_list(Extractors) ->
    Extractors.

test_desc(Mod) ->
    iolist_to_binary(io_lib:format("module '~w'", [Mod])).

all_test_cases(Extractors, Args, Opts) ->
    sort_test_cases(lists:flatten(lists:map(fun(Extractor) ->
        test_cases(Extractor, Extractor:chunks(Args), Opts)
    end, Extractors))).

test_cases(Extractor, Chunks, Opts) ->
    lists:filtermap(fun({Kind, Ln, Doc}) ->
        case {Extractor:code_blocks(Doc), Kind} of
            {{ok, CodeBlocks}, {doc, {M, F, A}, Tag}} ->
                case should_test_doc(Opts, {F, A}) of
                    true ->
                        {true, {Ln, doctest_eunit:doc_tests({M, F, A}, Ln, CodeBlocks, Tag)}};
                    false ->
                        false
                end;
            {{ok, CodeBlocks}, {moduledoc, M, Tag}} ->
                case should_test_moduledoc(Opts) of
                    true ->
                        {true, {Ln, doctest_eunit:moduledoc_tests(M, Ln, CodeBlocks, Tag)}};
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

should_test_doc(#{doc := true}, _Fun) ->
    true;
should_test_doc(#{doc := false}, _Fun) ->
    false;
should_test_doc(#{doc := Funs}, Fun) when is_list(Funs) ->
    lists:member(Fun, Funs);
should_test_doc(Opts, _Fun) when not is_map_key(doc, Opts) ->
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
