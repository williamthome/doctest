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
-module(doctest_md).
-moduledoc false.

% API functions
-export([code_blocks/1, code_block_asserts/2]).

-define(CODE_BLOCK_RE,
    "(?ms)^(```[`]*)erlang\\s*\\n" % ```erlang
    "(.*?)"                        %  <erlang-code>
    "(?:\\n^(\\1)(\\s+|\\n|$))"    %  ```
).

%%%=====================================================================
%%% API functions
%%%=====================================================================

% TODO: Check how to use shell_docs_markdown:parse_md/1.
%       It can simplify the capture of the code blocks,
%       but it's only available since OTP-27-rc3.
code_blocks(Markdown) when is_binary(Markdown) ->
    case re:run(Markdown, ?CODE_BLOCK_RE, [
        global, {capture, all_but_first, index}
    ]) of
        {match, Groups} ->
            {ok, [{binary_part(Markdown, Pos, Len), loc(Markdown, Pos)}
                 || [_, {Pos, Len}, _, _] <- Groups]};
        nomatch ->
            none
    end.

code_block_asserts(CodeBlock, Ln) ->
    asserts(chunks(split_lines(CodeBlock)), {Ln, Ln}, []).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

loc(Markdown, Pos) ->
    Pre = binary_part(Markdown, 0, Pos),
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

split_lines(CodeBlock) ->
    binary:split(CodeBlock, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).

chunks(Parts) ->
    Opts = [{capture, all_but_first, binary}],
    lists:map(fun(Part) ->
        case re:run(Part, <<"^[0-9]+>\\s+(.*?)\\.*$">>, Opts) of
            {match, [Left]} ->
                {left, Left};
            nomatch ->
                case re:run(Part, <<"^\\.\\.\\s+(.*?)\\.*$">>, Opts) of
                    {match, [More]} ->
                        {more, More};
                    nomatch ->
                        {right, Part}
                end
        end
    end, Parts).

% TODO: Maybe check for the correct line sequence by starting from 1, e.g.:
%       1> ok.
%       2> ok.
%       And this should be wrong:
%       9> error.
%       8> error.
asserts([{left, L}, {more, M} | T], {Ln, NLn}, Acc) ->
    asserts([{left, <<L/binary, M/binary>>} | T], {Ln, NLn+1}, Acc);
asserts([{left, L}, {right, R} | T], {Ln, NLn}, Acc) ->
    asserts(T, {NLn+2, NLn+2}, [{{L, R}, Ln} | Acc]);
asserts([], _, Acc) ->
    Acc;
% Code block is not a test, e.g:
% foo() ->
%     bar.
asserts(_, _, _) ->
    [].
