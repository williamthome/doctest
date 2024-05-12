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
-module(doctest_parse).
-moduledoc false.

% API functions
-export([module_tests/3]).

% Contains the docs_v1 record.
-include_lib("kernel/include/eep48.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

module_tests(Mod, ShouldTestModDoc, FunsOpts) ->
    case code:get_doc(Mod) of
        {ok, #docs_v1{anno = Anno, module_doc = Lang, docs = Docs}} ->
            Desc = iolist_to_binary(io_lib:format("module '~w'", [Mod])),
            case ShouldTestModDoc of
                true ->
                    {ok, {Desc, lists:flatten([
                        moduledoc_attr_tests(Mod, Anno, Lang),
                        doc_attr_tests(Mod, Docs, FunsOpts)
                    ])}};
                false ->
                    {ok, {Desc, doc_attr_tests(Mod, Docs, FunsOpts)}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

unwrap(#{<<"en">> := Markdown}) ->
    {ok, Markdown};
unwrap(none) ->
    none.

moduledoc_attr_tests(Mod, Anno, Lang) ->
    case unwrap(Lang) of
        {ok, Markdown} ->
            case doctest_md:code_blocks(Markdown) of
                {ok, CodeBlocks} ->
                    Ln = erl_anno:line(Anno),
                    doctest_eunit:moduledoc_tests(Mod, Ln, CodeBlocks);
                none ->
                    []
            end;
        none ->
            []
    end.

doc_attr_tests(Mod, Docs, FunsOpts) ->
    lists:filtermap(fun
        ({{function, Fun, Arity}, Anno, _Sign, Lang, _Meta}) ->
            case keep_fun({Fun, Arity}, FunsOpts) of
                true ->
                    case unwrap(Lang) of
                        {ok, Markdown} ->
                            case doctest_md:code_blocks(Markdown) of
                                {ok, CodeBlocks} ->
                                    MFA = {Mod, Fun, Arity},
                                    Ln = erl_anno:line(Anno),
                                    {true, doctest_eunit:doc_tests(MFA, Ln, CodeBlocks)};
                                none ->
                                    false
                            end;
                        none ->
                            false
                    end;
                false ->
                    false
            end;
        (_) ->
            false
    end, Docs).

keep_fun(_Fun, true) ->
    true;
keep_fun(_Fun, false) ->
    false;
keep_fun(Fun, Funs) when is_list(Funs) ->
    lists:member(Fun, Funs).
