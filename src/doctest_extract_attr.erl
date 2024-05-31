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
-module(doctest_extract_attr).
-behaviour(doctest_extract).

% doctest_extract callbacks
-export([chunks/1, code_blocks/1]).

-define(CODE_BLOCK_RE,
    "(?ms)^(```[`]*)erlang\\s*\\n" % ```erlang
    "(.*?)"                        % <erlang-code>
    "(?:\\n^(\\1)(\\s+|\\n|$))"    % ```
).

% Contains the docs_v1 record.
-include_lib("kernel/include/eep48.hrl").

%%%=====================================================================
%%% doctest_extract callbacks
%%%=====================================================================

chunks({Mod, Forms}) ->
    case code:get_doc(Mod) of
        {ok, #docs_v1{anno = Anno, module_doc = Lang, docs = Docs}} ->
            case moduledoc_chunk(Mod, Anno, Lang) of
                {ok, ModuleDocChunk} ->
                    [ModuleDocChunk | doc_chunks(Mod, Docs)];
                none ->
                    doc_chunks(Mod, Docs)
            end;
        {error, missing} ->
            doctest_error:raise(debug_info, [{Mod, Forms}], #{
                module => Mod,
                cause => "doctest requires 'debug_info' in compiler options"
            });
        {error, Reason} ->
            doctest_error:raise(Reason, [{Mod, Forms}], #{
                module => Mod,
                cause => Reason
            })
    end.

code_blocks(Markdown) ->
    doctest_extract:code_blocks(Markdown, ?CODE_BLOCK_RE).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

moduledoc_chunk(Mod, Anno, Lang) ->
    case unwrap(Lang) of
        {ok, Doc} ->
            {ok, {token(Mod), ln(Anno), Doc}};
        none ->
            none
    end.

doc_chunks(Mod, Docs) ->
    lists:filtermap(fun
        ({{function, Fun, Arity}, Anno, _Sign, Lang, _Meta}) ->
            case doc_chunk({Mod, Fun, Arity}, Anno, Lang) of
                {ok, Chunk} ->
                    {true, Chunk};
                none ->
                    false
            end;
        (_) ->
            false
    end, Docs).

doc_chunk(MFA, Anno, Lang) ->
    case unwrap(Lang) of
        {ok, Doc} ->
            {ok, {token(MFA), ln(Anno), Doc}};
        none ->
            none
    end.

token({Mod, Fun, Arity}) ->
    {doc, {Mod, Fun, Arity}, <<"-doc">>};
token(Mod) ->
    {moduledoc, Mod, <<"-moduledoc">>}.

ln(Anno) ->
    erl_anno:line(Anno).

% TODO: Language option.
unwrap(#{<<"en">> := Markdown}) ->
    {ok, Markdown};
unwrap(hidden) ->
    none;
unwrap(none) ->
    none.
