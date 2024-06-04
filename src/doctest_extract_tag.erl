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

% Contains the entry record.
-include_lib("edoc/src/edoc.hrl").

%%%=====================================================================
%%% doctest_extract callbacks
%%%=====================================================================

chunks({Mod, Forms}) ->
    Filename = doctest_forms:filename(Forms),
    Comments = erl_comment_scan:file(Filename),
    {Mod, _EDoc, Entries} =
        edoc_extract:source(Filename, edoc_lib:get_doc_env([]), [return_entries]),
    lists:filtermap(fun({Ln, Data}) ->
        case search_entry_comments(Ln, Comments) of
            {value, EntryComments} ->
                Doc = comments_to_binary(EntryComments),
                case Data of
                    moduledoc ->
                        {true, {token(Mod), Ln-1, Doc}};
                    {doc, {F, A}} ->
                        {true, {token({Mod, F, A}), Ln-1, Doc}}
                end;
            false ->
                false
        end
    end, filtermap_entries(Entries)).

code_blocks(Doc) ->
    doctest_extract:code_blocks(Doc, ?CODE_BLOCK_RE).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

filtermap_entries([#entry{data = []} | Entries]) ->
    filtermap_entries(Entries);
filtermap_entries([#entry{name = module, data = Data} | Entries]) ->
    [{entry_ln(Data), moduledoc} | filtermap_entries(Entries)];
filtermap_entries([#entry{name = {F, A}, data = Data} | Entries]) ->
    [{entry_ln(Data), {doc, {F, A}}} | filtermap_entries(Entries)];
filtermap_entries([_ | Entries]) ->
    filtermap_entries(Entries);
filtermap_entries([]) ->
    [].

entry_ln([#tag{name = doc, origin = comment, line = Ln} | _]) ->
    Ln;
entry_ln([_ | Elems]) ->
    entry_ln(Elems).

token({Mod, Fun, Arity}) ->
    {doc, {Mod, Fun, Arity}, <<"@doc">>};
token(Mod) ->
    {moduledoc, Mod, <<"@moduledoc">>}.

search_entry_comments(Ln, Comments) ->
    case lists:search(fun({CLn, _, _, _}) -> CLn =:= Ln end, Comments) of
        {value, {_, _, _, EntryComments}} ->
            {value, EntryComments};
        false ->
            false
    end.

comments_to_binary(Comments) ->
    iolist_to_binary(lists:join($\n, [rm_percentage(C) || C <- Comments])).

rm_percentage([$% | Cs]) -> rm_percentage(Cs);
rm_percentage([$\s | Cs]) -> Cs;
rm_percentage(Cs) -> Cs.
