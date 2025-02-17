%%%---------------------------------------------------------------------
%%% Copyright 2024-2025 William Fank Thomé
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
-module(doctest_eunit_report).

-behaviour(eunit_listener).

% API functions
-export([ start/0, start/1 ]).

% eunit_listener callbacks
-export([ init/1
        , handle_begin/3
        , handle_end/3
        , handle_cancel/3
        , terminate/2
        ]).

-record(state, {print_depth, groups}).

-ifndef(EUNIT_DEBUG_VAL_DEPTH).
-define(EUNIT_DEBUG_VAL_DEPTH, 15). % Default eunit.hrl value.
-endif.

%%%=====================================================================
%%% API functions
%%%=====================================================================

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

%%%=====================================================================
%%% eunit_listener callbacks
%%%=====================================================================

init(Opts) when is_list(Opts) ->
    #state{
       print_depth = proplists:get_value(print_depth, Opts, ?EUNIT_DEBUG_VAL_DEPTH),
       groups = orddict:new()
    }.

handle_begin(group, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    Group = [Data, orddict:new()],
    State#state{groups = orddict:append_list(Id, Group, Groups)};
handle_begin(test, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    GroupId = lists:droplast(Id),
    State#state{groups = orddict:update(GroupId, fun([Group, Tests]) ->
        case test_info(maps:from_list(Data)) of
            #{source := {_Mod, doctest_test, 0}} ->
                [Group, Tests];
            Test ->
                [Group, orddict:append(Id, Test, Tests)]
        end
    end, Groups)}.

handle_end(group, Data, State) ->
    {id, Id} = proplists:lookup(id, Data),
    State#state{groups = orddict:update(Id, fun([_Group, Tests]) ->
        [Data, Tests]
    end, State#state.groups)};
handle_end(test, Data, State) ->
    case proplists:get_value(status, Data) of
        {error, {error, {assertEqual, _Info}, _Stacktrace}} ->
            handle_end_1(Data, State);
        {error, {error, Reason, Stacktrace}} ->
            erlang:raise(error, Reason, Stacktrace);
        _Status ->
            handle_end_1(Data, State)
    end.

handle_end_1(Data, State) ->
    {id, Id} = proplists:lookup(id, Data),
    GroupId = lists:droplast(Id),
    State#state{groups = orddict:update(GroupId, fun([Group, Tests]) ->
        case orddict:is_key(Id, Tests) of
            true ->
                [Group, orddict:update(Id, fun([Test]) ->
                    [maps:merge(Test, maps:from_list(Data))]
                end, Tests)];
            false ->
                [Group, Tests]
        end
    end, State#state.groups)}.

handle_cancel(group, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    State#state{groups = orddict:update(Id, fun([_Group, Tests]) ->
        [Data, Tests]
    end, Groups)};
handle_cancel(test, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    GroupId = lists:droplast(Id),
    State#state{groups = orddict:update(GroupId, fun([Group, Tests]) ->
        case orddict:is_key(Id, Tests) of
            true ->
                [Group, orddict:update(Id, fun([Test]) ->
                    [maps:merge(Test, maps:from_list(Data))]
                end, Tests)];
            false ->
                [Group, Tests]
        end
    end, Groups)}.

terminate({ok, Data}, #state{groups = Groups} = State) ->
    print_header(),
    {ok, Time} = print_groups(Groups, State),
    print_footer(Data, Time),
    {ok, Data};
terminate({error, Reason}, State) ->
    print_error(error, Reason, [], State),
    {error, Reason}.

test_info(#{desc := <<"doctest ", Rest/binary>>} = D) ->
    [Tag, Title] = binary:split(Rest, <<$\s>>),
    [Filename, LnBin] = binary:split(Title, <<":">>),
    Ln = binary_to_integer(LnBin),
    D#{tag => Tag, title => Title, file => Filename, file_ln => Ln};
test_info(#{source := {M, F, A}} = D) when is_atom(M), is_atom(F), is_integer(A) ->
    Ln = guess_ln({M, F, A}),
    Title = doctest_eunit:test_title(M, Ln),
    [Filename, _LnBin] = binary:split(Title, <<":">>),
    Tag = iolist_to_binary(io_lib:format("~w/~p", [F, A])),
    D#{tag => Tag, title => Title, file => Filename, file_ln => Ln};
test_info(#{desc := <<"file \"", Rest/binary>>} = D) ->
    Size = byte_size(Rest) - 1,
    Filename = binary_part(Rest, 0, Size),
    D#{tag => <<"file">>, file => Filename};
test_info(#{desc := <<"directory \"", Rest/binary>>} = D) ->
    Size = byte_size(Rest) - 1,
    Filename = binary_part(Rest, 0, Size),
    D#{tag => <<"directory">>, file => Filename};
test_info(#{desc := <<"application \"", _Rest/binary>>} = D) ->
    % TODO: Get app filename
    D#{tag => <<"application">>, file => <<"./">>};
test_info(#{desc := undefined} = D) ->
    D#{tag => <<"unknown">>, file => none}.

guess_ln({M, F, A}) ->
    case module_forms(M) of
        {ok, Forms} ->
            case fun_line(Forms, F, A) of
                {ok, Ln} ->
                    Ln;
                error ->
                    0
            end;
        error ->
            0
    end.

module_forms(Mod) ->
    case code:which(Mod) of
        cover_compiled ->
            % There is a bug with the `code:get_doc` and it's not possible to return
            % the forms for now. After the fix, is possible to get the forms with:
            % > {file, Filename} = cover:is_compiled(Mod),
            % > do_module_forms(Filename);
            % See https://github.com/erlang/otp/pull/9433
            {ok, []};
        Filename ->
            do_module_forms(Filename)
    end.

do_module_forms(Filename) ->
    case beam_lib:chunks(Filename, [abstract_code]) of
        {ok, {_Mod, [{abstract_code, {_, Ast}}]}} ->
            {ok, Ast};
        {error, beam_lib, _} ->
            error
    end.

fun_line([{function, Anno, F, A, _} | _], F, A) ->
    {ok, erl_anno:line(Anno)};
fun_line([_ | T], F, A) ->
    fun_line(T, F, A);
fun_line([], _F, _A) ->
    error.

% TODO: Check if a heder should be printed.
print_header() ->
    ok.

print_groups(Groups, State) ->
    do_print_groups(enumerate(orddict:to_list(Groups)), 0, State).

print_footer(Data, Time) ->
    Order = [fail, pass, skip, cancel],
    Props = [proplists:lookup(Prop, Data) || Prop <- Order],
    Total = lists:foldl(fun({_, N}, Acc) -> Acc + N end, 0, Props),
    case Total =:= 0 of
        true ->
            ok;
        false ->
            Init = [{{fmt, "~p total", [Total]}, {fg, white}}],
            Info = lists:join(<<", ">>, lists:foldl(fun
                ({_, 0}, Acc) ->
                    Acc;
                ({fail, N}, Acc) ->
                    [{{fmt, "~p failed", [N]}, [bold, {fg, red}]} | Acc];
                ({pass, N}, Acc) ->
                    [{{fmt, "~p passed", [N]}, [bold, {fg, green}]} | Acc];
                ({skip, N}, Acc) ->
                    [{{fmt, "~p skipped", [N]}, [bold, {fg, yellow}]} | Acc];
                ({cancel, N}, Acc) ->
                    [{{fmt, "~p cancelled", [N]}, [{fg, bright_black}]} | Acc]
            end, Init, lists:reverse(Props))),
            doctest_term:write([
                <<"\n\n">>,
                {<<"Tests: ">>, bold}, Info,
                <<"\n">>,
                {<<" Time: ">>, bold}, {to_bin, Time / 1000}, <<" seconds">>,
                <<"\n\n">>
            ])
    end.

do_print_groups([{_I, {_Id, [_Data, []]}} | T], Time, State) ->
    do_print_groups(T, Time, State);
do_print_groups([{_I, {_Id, [Data, Tests]}} | T], Time, State) ->
    print_test(enumerate(orddict:to_list(Tests)), State),
    {time, GroupTime} = proplists:lookup(time, Data),
    do_print_groups(T, Time + GroupTime, State);
do_print_groups([], Time, _State) ->
    {ok, Time}.

print_test([{_I, {_Id, [#{status := ok} = Test]}} | T], State) ->
    print_test_pass(Test, State),
    print_output(Test, State),
    print_test(T, State);
print_test([{_I, {_Id, [#{status := {skipped, _Reason}} = Test]}} | T], State) ->
    print_test_skip(Test, State),
    print_output(Test, State),
    print_test(T, State);
print_test([{_I, {_Id, [#{status := {error, _Reason}} = Test]}} | T], State) ->
    print_test_fail(Test, State),
    print_output(Test, State),
    print_test(T, State);
print_test([], _State) ->
    ok.

print_test_pass(#{tag := Tag, title := Title}, _State) ->
    doctest_term:write([
        {<<" PASS ">>, [bold, {fg, white}, {bg, green}]},
        <<"\s">>,
        format_title(Title),
        <<"\s">>,
        format_tag(Tag)
    ]).

print_test_skip(#{tag := Tag, title := Title}, _State) ->
    doctest_term:write([
        {<<" SKIP ">>, [bold, {fg, white}, {bg, yellow}]},
        <<"\s">>,
        format_title(Title),
        <<"\s">>,
        format_tag(Tag)
    ]).

print_test_fail(#{tag := Tag, title := Title, status := {error, Reason}} = Test, State) ->
    doctest_term:write([
        {<<" FAIL ">>, [bold, {fg, white}, {bg, red}]},
        <<"\s">>,
        format_title(Title),
        <<"\s">>,
        format_tag(Tag),
        <<"\n\n">>
    | format_error(Reason, Test, State)]).

format_tag(Tag) ->
    {Tag, {fg, cyan}}.

format_title(Title) ->
    [Filename, Ln] = binary:split(Title, <<":">>),
    Dirname = filename:dirname(Filename),
    Basename = filename:basename(Filename, ".erl"),
    [
        {<<Dirname/binary, $/>>, {fg, bright_black}},
        {Basename, bold},
        {{fmt, ".erl:~s", [Ln]}, {fg, bright_black}}
    ].

print_output(#{output := Out}, _State) ->
    case iolist_to_binary(Out) of
        <<>> ->
            ok;
        Comment ->
            Pd = <<"\s\s\s">>,
            Lns = binary:split(Comment, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]),
            doctest_term:write([
                Pd, {<<"Output:\n\n">>, bold},
                lists:join(<<"\n">>, lists:map(fun(Ln) ->
                    [Pd, {Ln, {fg, bright_black}}]
                end, Lns))
            ])
    end.

format_error({error, {doctest, Reason}, Stacktrace}, _Test, _State) ->
    erlang:raise(error, {doctest, Reason}, Stacktrace);
format_error({error, {assertEqual, Info}, Stacktrace}, Test, State) ->
    case proplists:lookup(doctest, Info) of
        {doctest, DocTest} ->
            print_doctest(DocTest, {assertEqual, Info}, Test, Stacktrace, State);
        none ->
            print_test({assertEqual, Info}, Test, Stacktrace, State)
    end;
format_error({error, {Reason, Info}, Stacktrace}, Test, State) ->
    case is_list(Info) andalso (
        proplists:is_defined(expected, Info) orelse
        proplists:is_defined(pattern, Info)
    ) of
        true ->
            print_test({Reason, Info}, Test, Stacktrace, State);
        false ->
            print_error(error, {Reason, Info}, Stacktrace, State)
    end;
format_error({Class, Reason, Stacktrace}, _Test, State) ->
    print_error(Class, Reason, Stacktrace, State).

print_doctest(#{ln_range := {FromLn, ToLn}} = _DocTest, {ErrReason, Info}, Test, _Stack, State) ->
    Left = proplists:get_value(expected, Info),
    Right = proplists:get_value(value, Info),
    Filename = maps:get(file, Test),
    Lns = readlines(Filename),
    Pd = iolist_to_binary(lists:duplicate(byte_size(integer_to_binary(ToLn)), <<"\s">>)),
    Range = lists:map(fun({RLn, RExpr}) ->
        [<<"\s">>, {{to_bin, RLn}, {fg, red}}, <<"\s">>, {<<"│"/utf8>>, {fg, bright_black}}, <<"\s">>, RExpr, <<"\n">>]
    end, range(FromLn, ToLn, Lns)),
    [
        <<"\s">>, <<"❌\s"/utf8>>, {{to_bin, ErrReason}, {fg, bright_black}}, <<"\n">>,
        <<"\n">>,
        format_pre_code(Test, Pd),
        <<"\s">>, Pd, <<"\s">>, {<<"│"/utf8>>, {fg, bright_black}}, <<"\n">>,
        Range,
        <<"\s">>, Pd, <<"\s">>, {<<"│"/utf8>>, {fg, bright_black}}, <<"\n">>,
        <<"\s">>, Pd, <<"\s">>, {<<"└── at "/utf8>>, {fg, bright_black}}, {Filename, {fg, blue}}, {{fmt, ":~p", [FromLn]}, {fg, blue}}, <<"\n">>,
        <<"\n">>,
        <<"Expected:\n">>,
        {{fmt, "~tp", [Left]}, {fg, green}}, <<"\n">>,
        <<"\n">>,
        <<"Received:\n">>,
        {{fmt, "~tP", [Right, State#state.print_depth]}, {fg, red}}, <<"\n">>
    ].

print_test({Reason, Info}, Test, _Stacktrace, State) ->
    Left = proplists:get_value(expected, Info,
                proplists:get_value(pattern, Info)),
    Right = proplists:get_value(value, Info,
                proplists:get_value(unexpected_success, Info,
                    proplists:get_value(unexpected_exception, Info))),
    LeftFmt = case proplists:is_defined(pattern, Info) of
        true ->
            "~ts";
        false ->
            "~tp"
    end,
    RightFmt = "~tP",
    Filename = maps:get(file, Test),
    {line, Ln} = proplists:lookup(line, Info),
    Lns = readlines(Filename),
    LnExpr = lists:nth(Ln, Lns),
    Pd = iolist_to_binary(lists:duplicate(byte_size(integer_to_binary(Ln)), <<"\s">>)),
    [
        <<"\s">>, <<"❌\s"/utf8>>, {{to_bin, Reason}, {fg, bright_black}}, <<"\n">>,
        <<"\n">>,
        format_pre_code(Test, Pd),
        <<"\s">>, Pd, <<"\s">>, {<<"│"/utf8>>, {fg, bright_black}}, <<"\n">>,
        <<"\s">>, {{to_bin, Ln}, {fg, red}}, <<"\s">>, {<<"│"/utf8>>, {fg, bright_black}}, <<"\s">>, LnExpr, <<"\n">>,
        <<"\s">>, Pd, <<"\s">>, {<<"│"/utf8>>, {fg, bright_black}}, <<"\n">>,
        <<"\s">>, Pd, <<"\s">>, {<<"└── at "/utf8>>, {fg, bright_black}}, {Filename, {fg, blue}}, {{fmt, ":~p", [Ln]}, {fg, blue}}, <<"\n">>,
        <<"\n">>,
        <<"Expected:\n">>,
        {{fmt, LeftFmt, [Left]}, {fg, green}}, <<"\n">>,
        <<"\n">>,
        <<"Received:\n">>,
        {{fmt, RightFmt, [Right, State#state.print_depth]}, {fg, red}}, <<"\n">>
    ].

print_error(Class, Reason, Stacktrace, _State) ->
    [
        <<"\s\s\s❌\s"/utf8>>, {{to_bin, {Class, Reason}}, {fg, bright_black}}, <<"\n">>,
        <<"\n">>,
        <<"\s\s\s">>, {{fmt, "~tp", [Stacktrace]}, {fg, red}},
        <<"\n">>
    ].

readlines(Filename) ->
    {ok, Data} = file:read_file(Filename),
    binary:split(Data, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).

range(From, To, Lns) ->
    [H|T] = lists:nthtail(From-1, Lns),
    take(From, To, T, [{From, H}]).

take(To, To, _, Acc) ->
    lists:reverse(Acc);
take(From, To, [H|T], Acc) ->
    take(From+1, To, T, [{From+1, H}|Acc]).

format_pre_code(#{tag := "-moduledoc"}, Pd) ->
    {io_lib:format("\s~s\s-moduledoc\n", [Pd]), {fg, bright_black}};
format_pre_code(#{tag := "-doc"}, Pd) ->
    {io_lib:format("\s~s\s-doc\n", [Pd]), {fg, bright_black}};
format_pre_code(#{}, _Pd) ->
    "".

% Not using lists:enumerate from OTP just for compatibility reasons.
enumerate(List) ->
    do_enumerate(List, 1).

do_enumerate([H | T], I) ->
    [{I, H} | do_enumerate(T, I+1)];
do_enumerate([], _I) ->
    [].
