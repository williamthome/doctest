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
-module(doctest_parse_transform).
-moduledoc """
Generate tests for -doc attributes via parse transform.

Just plug the header on your module:
```
-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.
```
""".
-moduledoc #{ author => "William Fank Thomé [https://github.com/williamthome]" }.

% Check OTP version >= 27.
-include("doctest_otp_check.hrl").

% API functions
-export([parse_transform/2]).

% Settings that the user can override
-record(doctest, {enabled, funs}).

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opt) ->
    File = file(Forms),
    Docs = docs(doctest(doctest_attrs(Forms), File), doc_attrs(Forms)),
    run(tests(File, Forms, Docs)),
	Forms.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

run(Tests) ->
    eunit:test(Tests, [no_tty, {report, {eunit_progress, [colored]}}]).

docs(Doctest, AllDocs) ->
    case Doctest#doctest.funs of
        all ->
            AllDocs;
        Funs when is_list(Funs) ->
            maps:with(Funs, AllDocs)
    end.

doctest(Attrs, SrcFile) ->
    parse_to_doctest(Attrs, SrcFile, #doctest{
        enabled = true,
        funs = all
    }).

parse_to_doctest([Enabled|T], SrcFile, DT) when is_boolean(Enabled) ->
    parse_to_doctest(T, SrcFile, DT#doctest{enabled = Enabled});
parse_to_doctest([Funs|T], SrcFile, DT) when is_list(Funs) ->
    parse_to_doctest(T, SrcFile, DT#doctest{funs = Funs});
parse_to_doctest([all|T], SrcFile, DT) ->
    parse_to_doctest(T, SrcFile, DT#doctest{funs = all});
parse_to_doctest([Map|T], SrcFile, DT) when is_map(Map) ->
    parse_to_doctest(T, SrcFile, DT#doctest{
        enabled = maps:get(enabled, Map, DT#doctest.enabled),
        funs = maps:get(funs, Map, DT#doctest.funs)
    });
parse_to_doctest([], _SrcFile, #doctest{} = DT) ->
    DT.

tests(File, Forms, Docs) ->
    maps:fold(fun(Fun, Doc, Acc) ->
        case capture(Doc) of
            {match, Captured} ->
                [parse(File, Forms, Fun, rev_normalize(join(split(Captured)), [])) | Acc];
            nomatch ->
                Acc
        end
    end, [], Docs).

capture(Doc) ->
    case re:run(
        Doc,
        "(?ms)^(```[`]*)erlang\\s*\\n(.*?)(?:\\n^(\\1)(\\s+|\\n|$))",
        [global, {capture, all_but_first, binary}]
    ) of
        {match, [[_, Captured, _, _]]} ->
            {match, Captured};
        nomatch ->
            nomatch
    end.

split(Captured) ->
    binary:split(Captured, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).

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
%       ```erlang
%       1> ok.
%       2> ok.
%       ```
%       And this should be wrong:
%       ```erlang
%       9> error.
%       8> error.
%       ```
rev_normalize([{right, R}, {more, M}, {left, L} | T], Acc) ->
    rev_normalize(T, [{<<L/binary, $\s, M/binary>>, R} | Acc]);
rev_normalize([{right, R}, {more, MR}, {more, ML} | T], Acc) ->
    rev_normalize([{right, R}, {more, <<ML/binary, $\s, MR/binary>>} | T], Acc);
rev_normalize([{right, R}, {left, L} | T], Acc) ->
    rev_normalize(T, [{L, R} | Acc]);
rev_normalize([], Acc) ->
    Acc;
% Code block is not a test, e.g:
% ```erlang
% foo() ->
%     bar.
% ```
rev_normalize(_, _) ->
    [].

parse(File, Forms, {Fun, Arity, Line}, Asserts) ->
    {ok, Mod, Bin} = compile:forms(Forms, [
        {i, "eunit/include/eunit.hrl"}
    ]),
    {module, Mod} = code:load_binary(Mod, File, Bin),
    element(2, lists:foldl(fun({Left, Right}, {Bindings, Acc}) ->
        {LeftValue, NewBindings} = eval(Left, Bindings),
        {RightValue, []} = eval(Right, []),
        Test = {Line, fun() ->
            case LeftValue =:= RightValue of
                true ->
                    ok;
                false ->
                    erlang:error({assertEqual, [
                        {module, Mod},
                        {line, Line},
                        {function, Fun},
                        {arity, Arity},
                        {expression, Left},
                        {expected, RightValue},
                        {value, LeftValue}
                    ]})
            end
        end},
        {NewBindings, [Test | Acc]}
    end, {[], []}, Asserts)).

eval(Bin, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Bin/binary, $.>>)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
    {Value, NewBindings}.

file(Forms) ->
    [File, _Loc] = hd(attributes(file, Forms)),
    File.

doctest_attrs(Forms) ->
    attributes(doctest, Forms).

doc_attrs(Forms) ->
    do_doc_attrs(filtermap_forms(
        fun(Type) -> lists:member(Type, [attribute, function]) end,
        fun
            ({attribute, Attr}) ->
                case attribute_name(Attr) =:= doc of
                    true ->
                        {true, {doc, normalize_attribute(Attr)}};
                    false ->
                        false
                end;
            ({function, Fun}) ->
                {true, {function, normalize_function(Fun)}}
        end,
        Forms
    ), []).

do_doc_attrs([{doc, Doc},{function, Fun}|T], Acc) ->
    do_doc_attrs(T, [{Fun, Doc} | Acc]);
do_doc_attrs([{function, _}|T], Acc) ->
    do_doc_attrs(T, Acc);
do_doc_attrs([], Acc) ->
    maps:from_list(Acc).

attributes(Name, Forms) ->
    filtermap_forms(
        fun(Type) -> Type =:= attribute end,
        fun({attribute, Attr}) ->
            case attribute_name(Attr) =:= Name of
                true ->
                    {true, normalize_attribute(Attr)};
                false ->
                    false
            end
        end,
        Forms
    ).

filtermap_forms(Validate, Filtermap, Forms) when
    is_function(Validate, 1), is_function(Filtermap, 1), is_list(Forms) ->
    lists:filtermap(fun(Form) ->
        Type = erl_syntax:type(Form),
        case Validate(Type) of
            true ->
                Filtermap({Type, Form});
            false ->
                false
        end
    end, Forms).

attribute_name(Attr) ->
    erl_syntax:atom_value(erl_syntax:attribute_name(Attr)).

normalize_attribute(Attr) ->
    Exprs = erl_syntax:revert_forms(erl_syntax:attribute_arguments(Attr)),
    case lists:map(fun(Expr) ->
        {value, Value, []} = erl_eval:expr(Expr, []),
        Value
    end, Exprs) of
        [H] ->
            H;
        List ->
            List
    end.

normalize_function(Fun) ->
    {
        erl_syntax:atom_value(erl_syntax:function_name(Fun)),
        erl_syntax:function_arity(Fun),
        erl_anno:line(erl_syntax:get_pos(Fun))
    }.
