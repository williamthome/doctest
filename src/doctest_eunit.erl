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
-export([test/1, test/2]).

% Support functions
-export([moduledoc_tests/4, doc_tests/4, test_title/2]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

test(Tests) ->
    test(Tests, resolve).

test(Tests, resolve) ->
    eunit:test(Tests, options());
test(Tests, Options) when is_list(Options) ->
    eunit:test(Tests, Options).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

moduledoc_tests(Mod, AttrLn, CodeBlocks, Tag) ->
    case catch tests(Mod, AttrLn, CodeBlocks,
        fun({Left, LeftLn, LeftValue}, {Right, RightLn, RightValue}) ->
            {desc(Tag, Mod, LeftLn), {Mod, moduledoc, 0}, fun() ->
                case LeftValue =:= RightValue of
                    true ->
                        ok;
                    false ->
                        erlang:error({assertEqual, [
                            {doctest, #{
                                attribute => moduledoc,
                                tag => Tag,
                                left => {Left, LeftLn, LeftValue},
                                right => {Right, RightLn, RightValue},
                                ln_range => {LeftLn, RightLn}
                            }},
                            {module, Mod},
                            {line, LeftLn},
                            {expression, Left},
                            {expected, RightValue},
                            {value, LeftValue}
                        ]})
                end
            end}
        end
    ) of
        {ok, Tests} ->
            Tests;
        {error, {format, Info}} ->
            error({doctest, {moduledoc_format, Info#{
                module => Mod
            }}}, [Mod, AttrLn, CodeBlocks]);
        {error, {eval, Expr, Bindings, Stacktrace}} ->
            error({doctest, {moduledoc_eval, #{
                module => Mod,
                expression => Expr,
                bindings => Bindings,
                message => "Make sure to call Mod:Fun when testing "
                           "expressions via doctest:module"
            }}}, [Mod, AttrLn, CodeBlocks], [
                {error_info, #{
                    cause => Stacktrace,
                    module => Mod
                }}
            ])
    end.

doc_tests({M, F, A}, AttrLn, CodeBlocks, Tag) ->
    case catch tests(M, AttrLn, CodeBlocks,
        fun({Left, LeftLn, LeftValue}, {Right, RightLn, RightValue}) ->
            {desc(Tag, M, LeftLn), {M, F, A}, fun() ->
                case LeftValue =:= RightValue of
                    true ->
                        ok;
                    false ->
                        erlang:error({assertEqual, [
                            {doctest, #{
                                attribute => doc,
                                tag => Tag,
                                left => {Left, LeftLn, LeftValue},
                                right => {Right, RightLn, RightValue},
                                ln_range => {LeftLn, RightLn}
                            }},
                            {module, M},
                            {function, F},
                            {arity, A},
                            {line, LeftLn},
                            {expression, Left},
                            {expected, RightValue},
                            {value, LeftValue}
                        ]})
                end
            end}
        end
    ) of
        {ok, Tests} ->
            Tests;
        {error, {format, Info}} ->
            error({doctest, {doc_format, Info#{
                module => M,
                function => F,
                arity => A
            }}}, [{M, F, A}, AttrLn, CodeBlocks]);
        {error, {eval, Expr, Bindings, Stacktrace}} ->
            error({doctest, {doc_eval, #{
                module => M,
                function => F,
                arity => A,
                expression => Expr,
                bindings => Bindings,
                message => "Make sure to call Mod:Fun when testing "
                           "expressions via doctest:module"
            }}}, [{M, F, A}, AttrLn, CodeBlocks], [
                {error_info, #{
                    cause => Stacktrace,
                    module => M,
                    function => F
                }}
            ])
    end.

test_title(Mod, Ln) ->
    Filename = proplists:get_value(source, Mod:module_info(compile), code:which(Mod)),
    [[], Rel] = string:split(Filename, filename:absname("./")),
    iolist_to_binary(io_lib:format(".~s:~p", [Rel, Ln])).

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
    is_list(CodeBlocks), is_function(Callback, 2) ->
    {ok, lists:foldl(fun({CodeBlock, {CBLn, _CBCol}}, Acc) ->
        case code_block_asserts(CodeBlock, AttrLn + CBLn) of
            {ok, Asserts} ->
                lists:reverse(element(2, lists:foldl(fun({{Left, LeftLn}, {Right, RightLn}}, {Bindings, Acc1}) ->
                    {LeftValue, NewBindings} =
                        try eval(Left, Bindings, {value, fun(Name, Args) ->
                            % TODO: Maybe warn about testing non-exported functions.
                            % logger:warning(<<"testing a private function">>, #{
                            %     mfa => {Mod, Fun, Arity},
                            %     file => Filename,
                            %     line => LeftLn
                            % }),
                            erlang:apply(Mod, Name, Args)
                        end})
                        catch
                            error:undef:Stacktrace ->
                                throw({error, {eval, pp(Left), Bindings, Stacktrace}});
                            Class:Reason:Stacktrace ->
                                erlang:raise(Class, Reason, Stacktrace)
                        end,
                    {RightValue, []} = eval(Right, []),
                    Test = Callback(
                        {pp(Left), LeftLn, LeftValue},
                        {pp(Right), RightLn, RightValue}
                    ),
                    {NewBindings, [Test | Acc1]}
                end, {[], Acc}, lists:reverse(Asserts))));
            {error, Reason} ->
                throw({error, Reason})
        end
    end, [], CodeBlocks)}.

desc(Tag, Mod, Ln) ->
    iolist_to_binary(io_lib:format("doctest ~s\s~s", [Tag, test_title(Mod, Ln)])).

eval(Exprs, Bindings) ->
    eval(Exprs, Bindings, none).

eval(Exprs, Bindings, LocalFunctionHandler) ->
    {value, Value, NewBindings} = erl_eval:exprs(Exprs, Bindings, LocalFunctionHandler),
    {Value, NewBindings}.

pp(Exprs) ->
    iolist_to_binary(erl_pp:exprs(Exprs)).

code_block_asserts(CodeBlock, InitLn) ->
    case chunks(split_lines(CodeBlock)) of
        [] ->
            [];
        [H|_] = Chunks ->
            asserts(Chunks, {H, 1}, {InitLn, InitLn}, [])
    end.

split_lines(CodeBlock) ->
    binary:split(CodeBlock, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).

chunks(Parts) ->
    Opts = [{capture, all_but_first, binary}],
    lists:map(fun(Part) ->
        case re:run(Part, <<"^([1-9][0-9]*)>\\s(.*?)\\.*$">>, Opts) of
            {match, [N, Left]} ->
                {left, {N, Left}};
            nomatch ->
                case re:run(Part, <<"^(\\s*)\\.\\.\\s(.*?)\\.*$">>, Opts) of
                    {match, [Ws, More]} ->
                        {more, {Ws, More}};
                    nomatch ->
                        {right, Part}
                end
        end
    end, Parts).

asserts([{left, {N, L}}, {more, {Ws, M}} | T], HI, {Ln, NLn}, Acc) ->
    case check_more_format(Ws, N) of
        ok ->
            asserts([{left, {N, [scan(L), scan(M)]}} | T], HI, {Ln, NLn+1}, Acc);
        {error, {EWs, RWs}} ->
            Expected = iolist_to_binary([lists:duplicate(EWs, "\s"), "..> ", M]),
            Received = iolist_to_binary([lists:duplicate(RWs, "\s"), "..> ", M]),
            {error, {format, #{
                line => NLn+1,
                expected => Expected,
                received => Received
            }}}
    end;
asserts([{left, {N, L}}, {right, R} | T], {{left, {_, H}}, I}, {Ln, NLn}, Acc) ->
    case check_left_index(N, I) of
        ok when T =:= [] ->
            {ok, [{{parse(L), Ln}, {parse(R), NLn+1}} | Acc]};
        ok ->
            asserts(T, {hd(T), I+1}, {NLn+2, NLn+2}, [{{parse(L), Ln}, {parse(R), NLn+1}} | Acc]);
        error ->
            Expected = iolist_to_binary([integer_to_binary(I), "> ", H]),
            Received = iolist_to_binary([N, "> ", H]),
            {error, {format, #{
                line => Ln,
                expected => Expected,
                received => Received
            }}}
    end;
% Code block is not a test, e.g:
% foo() ->
%     bar.
asserts(_, _, _, _) ->
    {ok, []}.

scan(Expr) when is_binary(Expr) ->
    {ok, T, _} = erl_scan:string(binary_to_list(Expr)),
    T;
scan(Expr) ->
    Expr.

parse(Tokens) ->
    DotTokens = lists:flatten([scan(Tokens), scan(<<".">>)]),
    {ok, Exprs} = erl_parse:parse_exprs(DotTokens),
    Exprs.

check_more_format(Ws, Ln) ->
    LnSz = max(0, byte_size(Ln) - 1),
    WsSz = byte_size(Ws),
    case WsSz =:= LnSz of
        true ->
            ok;
        false ->
            {error, {LnSz, WsSz}}
    end.

check_left_index(N, Ln) ->
    case catch binary_to_integer(N) =:= Ln of
        true ->
            ok;
        _ ->
            error
    end.

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
