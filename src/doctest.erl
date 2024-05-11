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
-moduledoc """
Provides `module/1` and `module/2` to test doc attributes.
""".
-moduledoc #{ author => "William Fank Thomé [https://github.com/williamthome]" }.

% API functions
-export([module/1, module/2]).

% Support functions
-export([ code_blocks/1
        , chunk/1
        , parse_mod/3
        , parse_fun/3
        , parse/4
        , should_test_function/2
        ]).

% Check OTP version >= 27.
-include("doctest_otp_check.hrl").
-include_lib("kernel/include/eep48.hrl").

-type options() :: #{
    moduledoc => boolean(),
    funs => boolean() | [{atom(), arity()}]
}.
-type test_result() :: ok | error | {error, term()}.

%%%=====================================================================
%%% API functions
%%%=====================================================================

-doc #{ equiv => module(Mod, #{}) }.
-spec module(module()) -> test_result().

module(Mod) ->
    module(Mod, #{}).

-spec module(module(), options()) -> test_result().

module(Mod, Opts) when is_atom(Mod), is_map(Opts) ->
    case code:get_doc(Mod) of
        {ok, #docs_v1{anno = Anno, module_doc = Lang, docs = Docs}} ->
            ModTests = case maps:get(moduledoc, Opts, true) of
                true ->
                    moduledoc_tests(Mod, Anno, Lang);
                false ->
                    []
            end,
            FunsTests = doc_tests(Mod, Docs, maps:get(funs, Opts, true)),
            doctest_eunit:test(ModTests ++ FunsTests);
        {error, Reason} ->
            {error, Reason}
    end.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

code_blocks(Markdown) ->
    Pattern = "(?ms)^(```[`]*)erlang\\s*\\n(.*?)(?:\\n^(\\1)(\\s+|\\n|$))",
    Opts = [global, {capture, all_but_first, binary}],
    case re:run(Markdown, Pattern, Opts) of
        {match, Groups} ->
            {ok, lists:map(fun([_, CodeBlock, _, _]) ->
                CodeBlock
            end, Groups)};
        nomatch ->
            none
    end.

chunk(CodeBlock) ->
    rev_normalize(join(split(CodeBlock)), []).

parse_mod(M, Ln, CodeBlocks) ->
    parse(M, Ln, CodeBlocks, []).

parse_fun({M, F, A}, Ln, CodeBlocks) ->
    parse(M, Ln, CodeBlocks, [{function, F}, {arity, A}]).

parse(M, Ln, CodeBlocks, ErrInfo) when
    is_atom(M), is_integer(Ln), Ln > 0, is_list(CodeBlocks), is_list(ErrInfo) ->
    element(2, lists:foldl(fun(CodeBlock, Acc) ->
        lists:foldl(fun({Left, Right}, {Bindings, Acc1}) ->
            {LeftValue, NewBindings} = eval(Left, Bindings),
            {RightValue, []} = eval(Right, []),
            Test = {Ln, fun() ->
                case LeftValue =:= RightValue of
                    true ->
                        ok;
                    false ->
                        erlang:error({assertEqual, ErrInfo ++ [
                            {module, M},
                            {line, Ln},
                            {expression, Left},
                            {expected, RightValue},
                            {value, LeftValue}
                        ]})
                end
            end},
            {NewBindings, [Test | Acc1]}
        end, {[], Acc}, chunk(CodeBlock))
    end, [], CodeBlocks)).

should_test_function(true, _Fun) ->
    true;
should_test_function(false, _Fun) ->
    false;
should_test_function(Funs, Fun) when is_list(Funs) ->
    lists:member(Fun, Funs).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

moduledoc_tests(Mod, Anno, Lang) ->
    case code_blocks(unwrap_md(Lang)) of
        {ok, CodeBlocks} ->
            parse_mod(Mod, erl_anno:line(Anno), CodeBlocks);
        none ->
            []
    end.

doc_tests(Mod, Docs, Funs) ->
    lists:filtermap(fun({Type, Anno, _Sign, Lang, _Meta}) ->
        case Type of
            {function, Fun, Arity} ->
                case should_test_function(Funs, {Fun, Arity}) of
                    true ->
                        % TODO: Check how to use shell_docs_markdown:parse_md/1.
                        %       It can simplify the capture of the code blocks,
                        %       but it's only available since OTP-27-rc3.
                        case code_blocks(unwrap_md(Lang)) of
                            {ok, CodeBlocks} ->
                                MFA = {Mod, Fun, Arity},
                                Ln = erl_anno:line(Anno),
                                {true, parse_fun(MFA, Ln, CodeBlocks)};
                            none ->
                                false
                        end;
                    false ->
                        false
                end;
            _ ->
                false
        end
    end, Docs).

unwrap_md(#{<<"en">> := Md}) ->
    Md.

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
