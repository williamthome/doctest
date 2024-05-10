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
-module(doctest).
-moduledoc false.

% API functions
-export([module/1]).

% Support functions
-export([code_block/1, chunk/1, parse/3, run/1]).

% Check OTP version >= 27.
-include("doctest_otp_check.hrl").
-include_lib("kernel/include/eep48.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

module(M) ->
    case code:get_doc(M) of
        {ok, #docs_v1{docs = Docs}} ->
            Tests = lists:filtermap(fun({Type, Anno, _Sign, #{<<"en">> := Md}, _Meta}) ->
                case Type of
                    {function, F, A} ->
                        % TODO: Check how to use shell_docs_markdown:parse_md/1.
                        %       It can simplify the capture of the code blocks.
                        case code_block(Md) of
                            {ok, CodeBlock} ->
                                {true, parse({M, F, A}, erl_anno:line(Anno), chunk(CodeBlock))};
                            none ->
                                false
                        end;
                    _ ->
                        false
                end
            end, Docs),
            run(Tests);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

code_block(Md) ->
    case re:run(
        Md,
        "(?ms)^(```[`]*)erlang\\s*\\n(.*?)(?:\\n^(\\1)(\\s+|\\n|$))",
        [global, {capture, all_but_first, binary}]
    ) of
        {match, [[_, CodeBlock, _, _]]} ->
            {ok, CodeBlock};
        nomatch ->
            none
    end.

chunk(CodeBlock) ->
    rev_normalize(join(split(CodeBlock)), []).

parse({M, F, A}, Ln, Chunk) ->
    element(2, lists:foldl(fun({Left, Right}, {Bindings, Acc}) ->
        {LeftValue, NewBindings} = eval(Left, Bindings),
        {RightValue, []} = eval(Right, []),
        Test = {Ln, fun() ->
            case LeftValue =:= RightValue of
                true ->
                    ok;
                false ->
                    erlang:error({assertEqual, [
                        {module, M},
                        {line, Ln},
                        {function, F},
                        {arity, A},
                        {expression, Left},
                        {expected, RightValue},
                        {value, LeftValue}
                    ]})
            end
        end},
        {NewBindings, [Test | Acc]}
    end, {[], []}, Chunk)).

run(Tests) ->
    eunit:test(Tests, [no_tty, {report, {eunit_progress, [colored]}}]).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

split(CodeBlock) ->
    binary:split(CodeBlock, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).

join(Parts) ->
    Opts = [{capture, all_but_first, binary}],
    lists:foldl(fun(Part, Acc) ->
        case re:run(Part, <<"^[0-9]+>\\s+(.*?)\\.*$">>, Opts) of
            {match, [Left]} ->
                [{left, Left} | Acc];
            nomatch ->
                case re:run(Part, <<"^\\.\\.\\s+(.*?)\\.*$">>, Opts) of
                    {match, [More]} ->
                        [{more, More} | Acc];
                    nomatch ->
                        [{right, Part} | Acc]
                end
        end
    end, [], Parts).

% TODO: Maybe check for the correct line sequence by starting from 1, e.g.:
%       1> ok.
%       2> ok.
%       And this should be wrong:
%       9> error.
%       8> error.
rev_normalize([{right, R}, {more, M}, {left, L} | T], Acc) ->
    rev_normalize(T, [{<<L/binary, $\s, M/binary>>, R} | Acc]);
rev_normalize([{right, R}, {more, MR}, {more, ML} | T], Acc) ->
    rev_normalize([{right, R}, {more, <<ML/binary, $\s, MR/binary>>} | T], Acc);
rev_normalize([{right, R}, {left, L} | T], Acc) ->
    rev_normalize(T, [{L, R} | Acc]);
rev_normalize([], Acc) ->
    Acc;
% Code block is not a test, e.g:
% foo() ->
%     bar.
rev_normalize(_, _) ->
    [].

eval(Bin, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Bin/binary, $.>>)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
    {Value, NewBindings}.
