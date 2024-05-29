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
-module(doctest_eunit_report).
-moduledoc false.

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

-record(state, {groups}).

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

init(Options) when is_list(Options) ->
    #state{groups = orddict:new()}.

handle_begin(group, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    Group = [Data, orddict:new()],
    State#state{groups = orddict:append_list(Id, Group, Groups)};
handle_begin(test, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    GroupId = lists:droplast(Id),
    State#state{groups = orddict:update(GroupId, fun([Group, Tests]) ->
        Test = test_info(maps:from_list(Data)),
        [Group, orddict:append(Id, Test, Tests)]
    end, Groups)}.

handle_end(group, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    State#state{groups = orddict:update(Id, fun([_Group, Tests]) ->
        [Data, Tests]
    end, Groups)};
handle_end(test, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    GroupId = lists:droplast(Id),
    State#state{groups = orddict:update(GroupId, fun([Group, Tests]) ->
        [Group, orddict:update(Id, fun([Test]) ->
            [maps:merge(Test, maps:from_list(Data))]
        end, Tests)]
    end, Groups)}.

handle_cancel(group, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    State#state{groups = orddict:update(Id, fun([_Group, Tests]) ->
        [Data, Tests]
    end, Groups)};
handle_cancel(test, Data, #state{groups = Groups} = State) ->
    {id, Id} = proplists:lookup(id, Data),
    GroupId = lists:droplast(Id),
    State#state{groups = orddict:update(GroupId, fun([Group, Tests]) ->
        [Group, orddict:update(Id, fun([Test]) ->
            [maps:merge(Test, maps:from_list(Data))]
        end, Tests)]
    end, Groups)}.

terminate({ok, Data}, #state{groups = Groups} = _State) ->
    print_header(),
    {ok, Time} = print_groups(Groups),
    print_footer(Data, Time),
    {ok, Data};
terminate({error, Reason}, State) ->
    % TODO: Print error
    io:format(user, "[error] ~tp~n", [{Reason, State}]),
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
    D#{tag => ~"file", file => Filename};
test_info(#{desc := <<"directory \"", Rest/binary>>} = D) ->
    Size = byte_size(Rest) - 1,
    Filename = binary_part(Rest, 0, Size),
    D#{tag => ~"directory", file => Filename};
test_info(#{desc := <<"application \"", _Rest/binary>>} = D) ->
    % TODO: Get app filename
    D#{tag => ~"application", file => ~"./"};
test_info(#{desc := undefined} = D) ->
    D#{tag => ~"unknown", file => none}.

guess_ln({M, F, A}) ->
    case fun_line(mod_ast(M), F, A) of
        {ok, Ln} ->
            Ln;
        error ->
            0
    end.

mod_ast(Mod) ->
    {ok, {Mod, Chunks}} = beam_lib:chunks(code:which(Mod), [abstract_code]),
    [{abstract_code, {_, AST}}] = Chunks,
    AST.

fun_line([{function, Anno, F, A, _} | _], F, A) ->
    {ok, erl_anno:line(Anno)};
fun_line([_ | T], F, A) ->
    fun_line(T, F, A);
fun_line([], _F, _A) ->
    error.

% TODO: Check if a heder should be printed.
print_header() ->
    ok.

print_groups(Groups) ->
    do_print_groups(lists:enumerate(orddict:to_list(Groups)), 0).

print_footer(Data, Time) ->
    Order = [fail, pass, skip, cancel],
    Props = [proplists:lookup(Prop, Data) || Prop <- Order],
    Total = lists:foldl(fun({_, N}, Acc) -> Acc + N end, 0, Props),
    case Total =:= 0 of
        true ->
            ok;
        false ->
            Init = [{{fmt, "~p total", [Total]}, {fg, white}}],
            Info = lists:join(~", ", lists:foldl(fun
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
                ~"\n\n",
                {~"Tests: ", bold}, Info,
                ~"\n",
                {~" Time: ", bold}, {to_bin, Time / 1000}, ~" seconds",
                ~"\n\n"
            ])
    end.

do_print_groups([{_I, {_Id, [_Data, []]}} | T], Time) ->
    do_print_groups(T, Time);
do_print_groups([{_I, {_Id, [Data, Tests]}} | T], Time) ->
    print_test(lists:enumerate(orddict:to_list(Tests))),
    {time, GroupTime} = proplists:lookup(time, Data),
    do_print_groups(T, Time + GroupTime);
do_print_groups([], Time) ->
    {ok, Time}.

print_test([{_I, []} | T]) ->
    print_test(T);
print_test([{_I, {_Id, [#{status := ok} = Test]}} | T]) ->
    print_test_pass(Test),
    print_output(Test),
    print_test(T);
print_test([{_I, {_Id, [#{status := {skipped, _Reason}} = Test]}} | T]) ->
    print_test_skip(Test),
    print_output(Test),
    print_test(T);
print_test([{_I, {_Id, [#{status := {error, _Reason}} = Test]}} | T]) ->
    print_test_fail(Test),
    print_output(Test),
    print_test(T);
print_test([]) ->
    ok.

print_test_pass(#{tag := Tag, title := Title}) ->
    doctest_term:write([
        {~" PASS ", [bold, {fg, white}, {bg, green}]},
        ~"\s",
        format_title(Title),
        ~"\s",
        format_tag(Tag)
    ]).

print_test_skip(#{tag := Tag, title := Title}) ->
    doctest_term:write([
        {~" SKIP ", [bold, {fg, white}, {bg, yellow}]},
        ~"\s",
        format_title(Title),
        ~"\s",
        format_tag(Tag)
    ]).

print_test_fail(#{tag := Tag, title := Title, status := {error, Reason}} = Test) ->
    doctest_term:write([
        {~" FAIL ", [bold, {fg, white}, {bg, red}]},
        ~"\s",
        format_title(Title),
        ~"\s",
        format_tag(Tag),
        ~"\n\n"
    | format_error(Reason, Test)]).

format_tag(Tag) ->
    {Tag, {fg, cyan}}.

format_title(Title) ->
    [Filename, Ln] = binary:split(Title, <<":">>),
    Dirname = filename:dirname(Filename),
    Basename = filename:basename(Filename, ".erl"),
    [
        {Dirname, {fg, bright_black}}, ~"/",
        {Basename, bold},
        {{fmt, ".erl:~s", [Ln]}, {fg, bright_black}}
    ].

print_output(#{output := Out}) ->
    case iolist_to_binary(Out) of
        <<>> ->
            ok;
        Comment ->
            Lns = binary:split(Comment, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]),
            doctest_term:write([
                ~"\n\n",
                lists:join(~"\n", lists:map(fun(Ln) ->
                    [{~"%\s", {fg, bright_black}}, {Ln, {fg, bright_black}}]
                end, Lns))
            ])
    end.

format_error({error, {doctest, Error}, Stack}, _Test) ->
    erlang:raise(error, {doctest, Error}, Stack);
format_error({error, {assertEqual, Info}, Stack}, Test) ->
    case proplists:lookup(doctest, Info) of
        {doctest, DocTest} ->
            print_doctest(DocTest, {assertEqual, Info}, Test, Stack);
        none ->
            print_test({assertEqual, Info}, Test, Stack)
    end;
format_error({error, {Reason, Info}, Stack}, Test) ->
    print_test({Reason, Info}, Test, Stack).

print_doctest(#{ln_range := {FromLn, ToLn}} = _DocTest, {ErrReason, Info}, Test, _Stack) ->
    Left = proplists:get_value(expected, Info),
    Right = proplists:get_value(value, Info),

    Filename = maps:get(file, Test),
    Lns = readlines(Filename),
    Pd = iolist_to_binary(lists:duplicate(byte_size(integer_to_binary(ToLn)), ~"\s")),

    Range = lists:map(fun({RLn, RExpr}) ->
        [~"\s", {{to_bin, RLn}, {fg, red}}, ~"\s", {~"│", {fg, bright_black}}, ~"\s", RExpr, ~"\n"]
    end, range(FromLn, ToLn, Lns)),

    [
        ~"\s", Pd, ~"\s❌\s", {{to_bin, ErrReason}, {fg, bright_black}}, ~"\n",
        ~"\n",
        ~"\s", Pd, ~"\sExpected: ", {{fmt, "~tp", [Left]}, {fg, green}}, ~"\n",
        ~"\s", Pd, ~"\sReceived: ", {{fmt, "~tP", [Right, ?EUNIT_DEBUG_VAL_DEPTH]}, {fg, red}}, ~"\n",
        ~"\n",
        format_pre_code(Test, Pd),
        ~"\s", Pd, ~"\s", {~"│", {fg, bright_black}}, ~"\n",
        Range,
        ~"\s", Pd, ~"\s", {~"│", {fg, bright_black}}, ~"\n",
        ~"\s", Pd, ~"\s", {~"└── at ", {fg, bright_black}}, {Filename, {fg, blue}}, {{fmt, ":~p", [FromLn]}, {fg, blue}},
        ~"\n"
    ].

print_test({ErrReason, Info}, Test, _Stack) ->
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

    Pd = iolist_to_binary(lists:duplicate(byte_size(integer_to_binary(Ln)), ~"\s")),

    [
        ~"\s", Pd, ~"\s❌\s", {{to_bin, ErrReason}, {fg, bright_black}}, ~"\n",
        ~"\n",
        ~"\s", Pd, ~"\sExpected: ", {{fmt, LeftFmt, [Left]}, {fg, green}}, ~"\n",
        ~"\s", Pd, ~"\sReceived: ", {{fmt, RightFmt, [Right, ?EUNIT_DEBUG_VAL_DEPTH]}, {fg, red}}, ~"\n",
        ~"\n",
        format_pre_code(Test, Pd),
        ~"\s", Pd, ~"\s", {~"│", {fg, bright_black}}, ~"\n",
        ~"\s", {{to_bin, Ln}, {fg, red}}, ~"\s", {~"│", {fg, bright_black}}, ~"\s", LnExpr, ~"\n",
        ~"\s", Pd, ~"\s", {~"│", {fg, bright_black}}, ~"\n",
        ~"\s", Pd, ~"\s", {~"└── at ", {fg, bright_black}}, {Filename, {fg, blue}}, {{fmt, ":~p", [Ln]}, {fg, blue}},
        ~"\n"
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
