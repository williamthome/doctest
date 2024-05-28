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
-module(doctest_markdown).
-moduledoc false.
-behaviour(doctest_extract).

% API functions
-export([code_blocks/1, extract_module_tests/3, extract_forms_tests/2]).

-define(CODE_BLOCK_RE,
    "(?ms)^(```[`]*)erlang\\s*\\n" % ```erlang
    "(.*?)"                        % <erlang-code>
    "(?:\\n^(\\1)(\\s+|\\n|$))"    % ```
).

% Contains the parse_transform settings record.
-include("doctest_parse_transform.hrl").

% Contains the docs_v1 record.
-include_lib("kernel/include/eep48.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

code_blocks(Markdown) ->
    doctest_extract:code_blocks(Markdown, ?CODE_BLOCK_RE).

extract_module_tests(Mod, ShouldTestModDoc, FunsOpts) ->
    case code:get_doc(Mod) of
        {ok, #docs_v1{anno = Anno, module_doc = Lang, docs = Docs}} ->
            {ok, lists:flatten([
                moduledoc_attr_tests(ShouldTestModDoc, Mod, Anno, Lang),
                doc_attr_tests(FunsOpts, Mod, Docs)
            ])};
        {error, Reason} ->
            {error, Reason}
    end.

extract_forms_tests(Forms, Settings) ->
    DocAttrs = filter_doc_attrs(Settings, doc_attrs(Forms)),
    case compile:forms(Forms, [{i, "eunit/include/eunit.hrl"}]) of
        {ok, Mod, Bin} ->
            {module, Mod} = code:load_binary(Mod, file(Forms), Bin),
            Desc = iolist_to_binary(io_lib:format("module '~w'", [Mod])),
            {Desc, lists:flatten(lists:foldl(fun({Kind, {MarkdownLn, Markdown}}, Acc) ->
                case code_blocks(Markdown) of
                    {ok, CodeBlocks} ->
                        case Kind of
                            moduledoc ->
                                [doctest_eunit:moduledoc_tests(Mod, MarkdownLn, CodeBlocks)
                                | Acc];
                            {doc, {function, {F, A, _Ln}}} ->
                                [doctest_eunit:doc_tests({Mod, F, A}, MarkdownLn, CodeBlocks)
                                | Acc]
                        end;
                    none ->
                        Acc
                end
            end, [], DocAttrs))};
        error ->
            []
    end.

filter_doc_attrs(#settings{moduledoc = ShouldTestModDoc, funs = FunsOpts}, AllDocs) ->
    lists:filter(fun
        ({moduledoc, _Doc}) ->
            ShouldTestModDoc;
        ({{doc, {function, {F, A, _Ln}}}, _Doc}) ->
            doctest_extract:keep_fun({F, A}, FunsOpts)
    end, AllDocs).

doc_attrs(Forms) ->
    normalize_doc_attrs(doctest_parse_transform:filtermap_forms(
        fun(Type) -> lists:member(Type, [attribute, function]) end,
        fun
            ({attribute, Attr}) ->
                case doctest_parse_transform:attribute_name(Attr) of
                    moduledoc ->
                        case doctest_parse_transform:normalize_attribute(Attr) of
                            {Ln, Md} when is_list(Md); is_binary(Md) ->
                                {true, {moduledoc, {Ln, iolist_to_binary(Md)}}};
                            _ ->
                                false
                        end;
                    doc ->
                        case doctest_parse_transform:normalize_attribute(Attr) of
                            {Ln, Md} when is_list(Md); is_binary(Md) ->
                                {true, {doc, {Ln, iolist_to_binary(Md)}}};
                            _ ->
                                false
                        end;
                    _ ->
                        false
                end;
            ({function, Fun}) ->
                {true, {function, normalize_function(Fun)}}
        end,
        Forms
    ), []).

normalize_function(Fun) ->
    {
        erl_syntax:atom_value(erl_syntax:function_name(Fun)),
        erl_syntax:function_arity(Fun),
        doctest_parse_transform:form_line(Fun)
    }.

normalize_doc_attrs([{moduledoc, Doc} | T], Acc) ->
    normalize_doc_attrs(T, [{moduledoc, Doc} | Acc]);
normalize_doc_attrs([{doc, Doc},{function, Fun} | T], Acc) ->
    normalize_doc_attrs(T, [{{doc, {function, Fun}}, Doc} | Acc]);
normalize_doc_attrs([{function, _} | T], Acc) ->
    normalize_doc_attrs(T, Acc);
normalize_doc_attrs([], Acc) ->
    Acc.

file(Forms) ->
    [{_Ln, File}, _Loc] = hd(doctest_parse_transform:attributes(file, Forms)),
    File.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

moduledoc_attr_tests(false, _Mod, _Anno, _Lang) ->
    [];
moduledoc_attr_tests(true, Mod, Anno, Lang) ->
    case unwrap_codeblocks(Lang) of
        {ok, CodeBlocks} ->
            doctest_eunit:moduledoc_tests(
                Mod, erl_anno:line(Anno), CodeBlocks);
        none ->
            []
    end.

doc_attr_tests(false, _Mod, _Docs) ->
    [];
doc_attr_tests(Opts, Mod, Docs) ->
    lists:filtermap(fun
        ({{function, Fun, Arity}, Anno, _Sign, Lang, _Meta}) ->
            case doctest_extract:keep_fun({Fun, Arity}, Opts) andalso
                 unwrap_codeblocks(Lang)
            of
                {ok, CodeBlocks} ->
                    {true, doctest_eunit:doc_tests(
                        {Mod, Fun, Arity}, erl_anno:line(Anno), CodeBlocks)};
                _ ->
                    false
            end;
        (_) ->
            false
    end, Docs).

unwrap_codeblocks(Lang) ->
    case unwrap(Lang) of
        {ok, Markdown} ->
            code_blocks(Markdown);
        none ->
            none
    end.

unwrap(#{<<"en">> := Markdown}) ->
    {ok, Markdown};
unwrap(none) ->
    none.
