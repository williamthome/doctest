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

% API functions
-export([test/1, test/2]).

% Support functions
-export([moduledoc_tests/4, doc_tests/4, test_title/2]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

test(Tests) ->
    test(Tests, rebar3_config).

test(ignore, _Opts) ->
    ok;
test(Tests, rebar3_config) ->
    eunit:test({inparallel, Tests}, rebar3_config_opts());
test(Tests, Options) when is_list(Options) ->
    eunit:test({inparallel, Tests}, Options).

%%%=====================================================================
%%% Support functions
%%%=====================================================================

moduledoc_tests(Mod, AttrLn, CodeBlocks, Tag) ->
    case catch tests(Mod, AttrLn, CodeBlocks,
        fun({Left, LeftLn, LeftValue}, {Right, RightLn, RightValue}) ->
            {desc(Tag, Mod, LeftLn), {Mod, moduledoc, 0}, fun() ->
                case LeftValue of
                    RightValue ->
                        ok;
                    _ ->
                        error({assertEqual, [
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
            error({doctest, {format, Info}}, [Mod, AttrLn, CodeBlocks], [
                {error_info, Info#{
                    attribute => moduledoc,
                    module => Mod,
                    cause => format
                }}
            ]);
        {error, {eval, Expr, Ln, Bindings, Reason}} ->
            error({doctest, eval, {Expr, Ln, Bindings, Reason}}, [Mod, AttrLn, CodeBlocks], [
                {error_info, #{
                    attribute => moduledoc,
                    module => Mod,
                    expression => Expr,
                    bindings => Bindings,
                    cause => Reason,
                    line => Ln
                }}
            ]);
        {error, {parse, Expr, Ln, ErrInfo}} ->
            error({doctest, eval, {Expr, Ln, ErrInfo}}, [Mod, AttrLn, CodeBlocks], [
                {error_info, #{
                    attribute => moduledoc,
                    module => Mod,
                    expression => Expr,
                    cause => parse,
                    line => Ln
                }}
            ])
    end.

doc_tests({M, F, A}, AttrLn, CodeBlocks, Tag) ->
    case catch tests(M, AttrLn, CodeBlocks,
        fun({Left, LeftLn, LeftValue}, {Right, RightLn, RightValue}) ->
            {desc(Tag, M, LeftLn), {M, F, A}, fun() ->
                case LeftValue of
                    RightValue ->
                        ok;
                    _ ->
                        error({assertEqual, [
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
            error({doctest, {format, Info}}, [{M, F, A}, AttrLn, CodeBlocks], [
                {error_info, Info#{
                    attribute => doc,
                    module => M,
                    function => F,
                    arity => A,
                    cause => format
                }}
            ]);
        {error, {eval, Expr, Ln, Bindings, Reason}} ->
            error({doctest, {eval, Expr, Ln, Bindings, Reason}}, [{M, F, A}, AttrLn, CodeBlocks], [
                {error_info, #{
                    attribute => doc,
                    module => M,
                    function => F,
                    arity => A,
                    expression => Expr,
                    bindings => Bindings,
                    cause => Reason,
                    line => Ln
                }}
            ]);
        {error, {parse, Expr, Ln, ErrInfo}} ->
            error({doctest, parse, Expr, ErrInfo}, [{M, F, A}, AttrLn, CodeBlocks], [
                {error_info, #{
                    attribute => doc,
                    module => M,
                    function => F,
                    arity => A,
                    expression => Expr,
                    cause => parse,
                    line => Ln
                }}
            ])
    end.

test_title(Mod, Ln) ->
    Filename = proplists:get_value(source, Mod:module_info(compile), code:which(Mod)),
    case string:split(Filename, filename:absname("./")) of
        [[], Rel] ->
            iolist_to_binary(io_lib:format(".~s:~p", [Rel, Ln]));
        Rel ->
            iolist_to_binary(io_lib:format(".~s:~p", [Rel, Ln]))
    end.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

rebar3_config_opts() ->
    case erlang:module_loaded(rebar3) of
        true ->
            resolve_eunit_opts(rebar3:init_config());
        false ->
            []
    end.

tests(Mod, AttrLn, CodeBlocks, Callback) when
    is_atom(Mod), is_integer(AttrLn), AttrLn >= 0,
    is_list(CodeBlocks), is_function(Callback, 2) ->
    {ok, lists:foldl(fun({CodeBlock, {CBLn, _CBCol}}, Acc) ->
        case code_block_asserts(CodeBlock, AttrLn + CBLn) of
            {ok, Asserts} ->
                lists:reverse(element(2, lists:foldl(
                    fun({{Left, LeftLn}, {Right, RightLn}}, {Bindings, Acc1}) ->
                        LocalFunctionHandler = {value, fun(Name, Args) ->
                            erlang:apply(Mod, Name, Args)
                        end},
                        {LeftValue, NewBindings} = eval(Left, LeftLn, Bindings, LocalFunctionHandler),
                        % Skip when no right value
                        case Right of
                            [{var, _, '_'}] ->
                                {NewBindings, Acc1};
                            _ ->
                                {RightValue, _} = eval(Right, RightLn, []),
                                Test = Callback(
                                    {pp(Left), LeftLn, LeftValue},
                                    {pp(Right), RightLn, RightValue}
                                ),
                                {NewBindings, [Test | Acc1]}
                        end
                    end, {[], Acc}, lists:reverse(Asserts)
                )));
            {error, Reason} ->
                throw({error, Reason})
        end
    end, [], CodeBlocks)}.

desc(Tag, Mod, Ln) ->
    iolist_to_binary(io_lib:format("doctest ~s\s~s", [Tag, test_title(Mod, Ln)])).

eval(Exprs, Ln, Bindings) ->
    eval(Exprs, Ln, Bindings, none).

eval(Exprs, Ln, Bindings, LocalFunctionHandler) ->
    try
        {value, Value, NewBindings} =
            erl_eval:exprs(Exprs, Bindings, LocalFunctionHandler),
        {Value, NewBindings}
    catch
        _Class:Reason:_Stacktrace ->
            throw({error, {eval, pp(Exprs), Ln, Bindings, Reason}})
    end.

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
    {ok, ReExpr} = re:compile(<<"^([1-9][0-9]*)?>\\s(.*?)\\.*$">>),
    {ok, ReMoreExpr} = re:compile(<<"^(\\s*)(\\.*)\\s(.*?)\\.*$">>),
    do_chunks(Parts, ReExpr, ReMoreExpr).

do_chunks([], _ReExpr, _ReMoreExpr) ->
    [];
do_chunks([H | T], ReExpr, ReMoreExpr) ->
    case do_chunk(H, ReExpr, ReMoreExpr) of
        {right, R0} ->
            {R, T1} = find_right_side_end(T, ReExpr, R0),
            [{right, R} | do_chunks(T1, ReExpr, ReMoreExpr)];
        Chunk ->
            [Chunk | do_chunks(T, ReExpr, ReMoreExpr)]
    end.

do_chunk(Part, ReExpr, ReMoreExpr) ->
    ReOpts = [{capture, all_but_first, binary}],
    case re:run(Part, ReExpr, ReOpts) of
        {match, [<<>>, Left]} ->
            {left, {undefined, Left}};
        {match, [N, Left]} ->
            {left, {N, Left}};
        nomatch ->
            case re:run(Part, ReMoreExpr, ReOpts) of
                {match, [Ws, Dots, More]} ->
                    {more, {Ws, Dots, More}};
                nomatch ->
                    {right, Part}
            end
    end.

find_right_side_end([], _ReExpr, Acc) ->
    {Acc, []};
find_right_side_end([H | T] = All, ReExpr, Acc) ->
    case re:run(H, ReExpr, [{capture, none}]) of
        match ->
            {Acc, All};
        nomatch ->
            find_right_side_end(T, ReExpr, <<Acc/binary, $\n, H/binary>>)
    end.

asserts([{left, {N, L}}, {more, {Ws, Dots, M}} | T], HI, {Ln, NLn}, Acc) ->
    case check_more_format(N, Ws) of
        ok ->
            asserts([{left, {N, <<L/binary, $\n, M/binary>>}} | T], HI, {Ln, NLn+1}, Acc);
        {error, {EWs, RWs}} ->
            Expected = iolist_to_binary([lists:duplicate(EWs, "\s"), Dots, "> ", M]),
            Received = iolist_to_binary([lists:duplicate(RWs, "\s"), Dots, "> ", M]),
            {error, {format, #{
                line => NLn+1,
                expected => Expected,
                received => Received
            }}}
    end;
asserts([{left, {N, L}}, {right, R} | T], {{left, {_, H}}, I}, {Ln, NLn}, Acc) ->
    case check_left_index(N, I) of
        ok when T =:= [] ->
            {ok, [{{parse(L, Ln), Ln}, {parse(R, NLn+1), NLn+1}} | Acc]};
        ok ->
            asserts(T, {hd(T), I+1}, {NLn+2, NLn+2}, [{{parse(L, Ln), Ln}, {parse(R, NLn+1), NLn+1}} | Acc]);
        error ->
            Expected = iolist_to_binary([integer_to_binary(I), "> ", H]),
            Received = iolist_to_binary([case N of undefined -> <<>>; _ -> N end, "> ", H]),
            {error, {format, #{
                line => Ln,
                expected => Expected,
                received => Received
            }}}
    end;
asserts([{left, _} = Left, {left, _} = Next | T], HI, {Ln, NLn}, Acc) ->
    asserts([Left, {right, <<"_">>}, Next | T], HI, {Ln-1, NLn-1}, Acc);
% Code block is not a test, e.g:
% foo() ->
%     bar.
asserts(_, _, _, _) ->
    {ok, []}.

parse(Expr, Ln) ->
    try
        {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Expr/binary, $.>>)),
        {ok, Ast} = erl_parse:parse_exprs(Tokens),
        Ast
    catch
        error:ErrInfo ->
            throw({error, {parse, Expr, Ln, ErrInfo}})
    end.

check_more_format(undefined, _Ws) ->
    ok;
check_more_format(Ln, Ws) ->
    LnSz = max(0, byte_size(Ln) - 1),
    WsSz = byte_size(Ws),
    case WsSz =:= LnSz of
        true ->
            ok;
        false ->
            {error, {LnSz, WsSz}}
    end.

check_left_index(undefined, _Ln) ->
    ok;
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
