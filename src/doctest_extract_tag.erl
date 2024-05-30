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
-module(doctest_extract_tag).
-behaviour(doctest_extract).

% API functions
-export([chunks/1, code_blocks/1]).

-define(CODE_BLOCK_RE,
    "(?ms)^(```)\\s*\\n"        % ```
    "(.*?)"                     % <erlang-code>
    "(?:\\n^(''')(\\s+|\\n|$))" % ''')
).

%%%=====================================================================
%%% API functions
%%%=====================================================================

chunks({Mod, Forms}) ->
    Filename = doctest_forms:filename(Forms),
    Comments = erl_comment_scan:file(Filename),
    Tree = erl_recomment:recomment_forms(Forms, Comments),
    do_chunks(Mod, Tree).

code_blocks(Doc) ->
    doctest_extract:code_blocks(Doc, ?CODE_BLOCK_RE).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_chunks(Mod, Tree) ->
    Forms = erl_syntax:form_list_elements(erl_syntax:flatten_form_list(Tree)),
    do_chunks_1(Forms, Mod).

do_chunks_1([Form | Forms], Mod) ->
    case erl_syntax:get_precomments(Form) of
        [] ->
            do_chunks_1(Forms, Mod);
        Comments ->
            do_chunks_2(Form, Forms, Mod, Comments)
    end;
do_chunks_1([], _Mod) ->
    [].

do_chunks_2(Form, Forms, Mod, Comments) ->
    case erl_syntax_lib:analyze_form(Form) of
        {function, {Fun, Arity}} ->
            {Ln, Doc} = comment_text(Comments),
            [{{doc, {Mod, Fun, Arity}, <<"@doc">>}, Ln-1, Doc} | do_chunks_1(Forms, Mod)];
        {attribute, {module, Mod}} ->
            {Ln, Doc} = comment_text(Comments),
            [{{moduledoc, Mod, <<"@moduledoc">>}, Ln-1, Doc} | do_chunks_1(Forms, Mod)];
        _ ->
            do_chunks_1(Forms, Mod)
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
