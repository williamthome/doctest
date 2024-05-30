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

% doctest_extract callbacks
-export([chunks/1, code_blocks/1]).

-define(CODE_BLOCK_RE,
    "(?ms)^(```)\\s*\\n"        % ```
    "(.*?)"                     % <erlang-code>
    "(?:\\n^(''')(\\s+|\\n|$))" % ''')
).

%%%=====================================================================
%%% doctest_extract callbacks
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
            [chunk({Mod, Fun, Arity}, Comments) | do_chunks_1(Forms, Mod)];
        {attribute, {module, Mod}} ->
            [chunk(Mod, Comments) | do_chunks_1(Forms, Mod)];
        _ ->
            do_chunks_1(Forms, Mod)
    end.

chunk(Data, Comments) ->
    {token(Data), ln(Comments), doc(Comments)}.

token({Mod, Fun, Arity}) ->
    {doc, {Mod, Fun, Arity}, <<"@doc">>};
token(Mod) ->
    {moduledoc, Mod, <<"@moduledoc">>}.

doc(Comments) ->
    iolist_to_binary(lists:join($\n, lists:map(fun(C) ->
        lists:join($\n, [rm_percentage(T) || T <- erl_syntax:comment_text(C)])
    end, Comments))).

rm_percentage([$% | Cs]) -> rm_percentage(Cs);
rm_percentage([$\s | Cs]) -> Cs;
rm_percentage(Cs) -> Cs.

ln([H|_]) ->
    erl_anno:line(erl_syntax:get_pos((H))) - 1.
