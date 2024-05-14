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
-module(doctest_eunit).
-moduledoc false.

% API functions
-export([test/1, test/2, moduledoc_tests/3, doc_tests/3]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

test(Tests) ->
    test(Tests, resolve).

test(Tests, resolve) ->
    eunit:test(Tests, options());
test(Tests, Options) when is_list(Options) ->
    eunit:test(Tests, Options).

moduledoc_tests(Mod, AttrLn, CodeBlocks) ->
    tests(Mod, AttrLn, CodeBlocks,
        fun(Ln, {Left, LeftValue}, {_Right, RightValue}) ->
            Desc = iolist_to_binary(
                io_lib:format("-moduledoc | Ln ~p", [Ln])
            ),
            {Desc, Ln, fun() ->
                case LeftValue =:= RightValue of
                    true ->
                        ok;
                    false ->
                        erlang:error({assertEqual, [
                            {doctest, [
                                {attribute, moduledoc}
                            ]},
                            {module, Mod},
                            {line, Ln},
                            {expression, Left},
                            {expected, RightValue},
                            {value, LeftValue}
                        ]})
                end
            end}
        end
    ).

doc_tests({M, F, A}, AttrLn, CodeBlocks) ->
    tests(M, AttrLn, CodeBlocks,
        fun(Ln, {Left, LeftValue}, {_Right, RightValue}) ->
            Desc = iolist_to_binary(
                io_lib:format("-doc | Ln ~p", [Ln])
            ),
            {Desc, {M, F, A}, fun() ->
                case LeftValue =:= RightValue of
                    true ->
                        ok;
                    false ->
                        erlang:error({assertEqual, [
                            {doctest, [
                                {attribute, doc}
                            ]},
                            {module, M},
                            {function, F},
                            {arity, A},
                            {line, Ln},
                            {expression, Left},
                            {expected, RightValue},
                            {value, LeftValue}
                        ]})
                end
            end}
        end
    ).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

options() ->
    case erlang:module_loaded(rebar3) of
        true ->
            resolve_eunit_opts(rebar3:init_config());
        false ->
            []
    end.

tests(Mod, AttrLn, CodeBlocks, Callback) when
    is_atom(Mod), is_integer(AttrLn), AttrLn > 0,
    is_list(CodeBlocks), is_function(Callback, 3) ->
    lists:foldl(fun({CodeBlock, {CBLn, _CBCol}}, Acc) ->
        element(2, lists:foldl(fun({{Left, Right}, Ln}, {Bindings, Acc1}) ->
            {LeftValue, NewBindings} = eval(Left, Bindings),
            {RightValue, []} = eval(Right, []),
            Test = Callback(Ln, {Left, LeftValue}, {Right, RightValue}),
            {NewBindings, [Test | Acc1]}
        end, {[], Acc}, code_block_asserts(CodeBlock, AttrLn + CBLn)))
    end, [], CodeBlocks).

code_block_asserts(CodeBlock, Ln) ->
    asserts(chunks(lines(CodeBlock)), {Ln, Ln}, []).

lines(CodeBlock) ->
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

eval(Bin, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Bin/binary, $.>>)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
    {Value, NewBindings}.

%%%=====================================================================
%%% rebar3 non-exported functions
%%%=====================================================================

% TODO: Maybe submit a PR exporting 'resolve_eunit_opts/1'.
%       See https://github.com/erlang/rebar3/blob/b64d94f4e6fb738c4a3004faf833e0b9617d86a8/apps/rebar/src/rebar_prv_eunit.erl#L443

resolve_eunit_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    EUnitOpts = rebar_state:get(State, eunit_opts, []),
    EUnitOpts1 = case proplists:get_value(verbose, Opts, false) of
                    true  -> set_verbose(EUnitOpts);
                    false -> EUnitOpts
                 end,
    EUnitOpts2 = case proplists:get_value(profile, Opts, false) of
                    true  -> set_profile(EUnitOpts1);
                    false -> EUnitOpts1
                 end,
    IsVerbose = lists:member(verbose, EUnitOpts2),
    case proplists:get_value(eunit_formatters, Opts, not IsVerbose) of
        true  -> custom_eunit_formatters(EUnitOpts2);
        false -> EUnitOpts2
    end.

custom_eunit_formatters(Opts) ->
    ReportOpts = custom_eunit_report_options(Opts),
    %% If `report` is already set then treat that like `eunit_formatters` is false
    case lists:keymember(report, 1, Opts) of
        true -> Opts;
        false -> [no_tty, {report, {eunit_progress, ReportOpts}} | Opts]
    end.

custom_eunit_report_options(Opts) ->
    case lists:member(profile, Opts) of
        true -> [colored, profile];
        false -> [colored]
    end.

set_profile(Opts) ->
    %% if `profile` is already set don't set it again
    case lists:member(profile, Opts) of
        true  -> Opts;
        false -> [profile] ++ Opts
    end.

set_verbose(Opts) ->
    %% if `verbose` is already set don't set it again
    case lists:member(verbose, Opts) of
        true  -> Opts;
        false -> [verbose] ++ Opts
    end.
