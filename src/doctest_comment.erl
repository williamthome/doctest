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
-module(doctest_comment).
-moduledoc false.
-behaviour(doctest_extract).

% API functions
-export([code_blocks/1, extract_module_tests/3, extract_forms_tests/2]).

-define(CODE_BLOCK_RE,
    "(?ms)^(```)\\s*\\n"        % ```
    "(.*?)"                     % <erlang-code>
    "(?:\\n^(''')(\\s+|\\n|$))" % ''')
).

% Contains the parse_transform settings record.
-include("doctest_parse_transform.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

code_blocks(Doc) ->
    doctest_extract:code_blocks(Doc, ?CODE_BLOCK_RE).

extract_module_tests(Mod, ShouldTestModDoc, FunsOpts) ->
    {source, Filename} = proplists:lookup(source, Mod:module_info(compile)),
    {ok, Forms} = epp:parse_file(Filename, [], []),
    Comments = erl_comment_scan:file(Filename),
    Tree = erl_recomment:recomment_forms(Forms, Comments),
    {ok, do_module_extract_tests(Mod, ShouldTestModDoc, FunsOpts, Tree)}.

extract_forms_tests(_Forms, _Settings) ->
    error(not_implemented_yet).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_module_extract_tests(Mod, ShouldTestModDoc, FunsOpts, Tree) ->
    Forms = erl_syntax:form_list_elements(erl_syntax:flatten_form_list(Tree)),
    do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts).

do_module_extract_tests_1([Form | Forms], Mod, ShouldTestModDoc, FunsOpts) ->
    case erl_syntax:get_precomments(Form) of
        [] ->
            do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts);
        Comments ->
            {Ln, Doc} = comment_text(Comments),
            case code_blocks(Doc) of
                {ok, CodeBlocks} ->
                    do_module_extract_tests_2(Form, Forms, Mod, ShouldTestModDoc, FunsOpts, Ln, CodeBlocks);
                none ->
                    do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts)
            end
    end;
do_module_extract_tests_1([], _Mod, _ShouldTestModDoc, _FunsOpts) ->
    [].

do_module_extract_tests_2(Form, Forms, Mod, ShouldTestModDoc, FunsOpts, Ln, CodeBlocks) ->
    case erl_syntax_lib:analyze_form(Form) of
        {function, {Fun, Arity}} ->
            case doctest_extract:keep_fun({Fun, Arity}, FunsOpts) of
                true ->
                    [doctest_eunit:doc_tests({Mod, Fun, Arity}, Ln, CodeBlocks)
                    | do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts)];
                false ->
                    do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts)
            end;
        {attribute, {module, Mod}} ->
            case ShouldTestModDoc of
                true ->
                    [doctest_eunit:moduledoc_tests(Mod, Ln, CodeBlocks)
                    | do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts)];
                false ->
                    do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts)
            end;
        _ ->
            do_module_extract_tests_1(Forms, Mod, ShouldTestModDoc, FunsOpts)
    end.

comment_text(Cs) ->
    {get_line(hd(Cs)), iolist_to_binary(lists:join($\n, lists:map(fun(C) ->
        lists:join($\n, [remove_percent_chars(S) || S <- erl_syntax:comment_text(C)])
    end, Cs)))}.

get_line(Tree) ->
    Anno = erl_syntax:get_pos(Tree),
    erl_anno:line(Anno).

remove_percent_chars([$% | Cs]) -> remove_percent_chars(Cs);
remove_percent_chars([$\s | Cs]) -> Cs;
remove_percent_chars(Cs) -> Cs.
