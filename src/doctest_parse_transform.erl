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
-record(doctest, {enabled, location, module, funs}).

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opt) ->
    ok = generate_doctest_file(Forms),
	Forms.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

%% ---------------------------------------------------------------------
%% Main

generate_doctest_file(Forms) ->
    Attrs = doctest_attrs(Forms),
    SrcMod = src_module(Forms),
    SrcFile = src_filename(Forms),
    Doctest = doctest(Attrs, SrcMod, SrcFile),

    AllDocs = doc_attrs(Forms),
    Docs = filter_funs_docs(Doctest, AllDocs),
    Asserts = docs_asserts(Docs),

    output(Doctest, SrcMod, SrcFile, Asserts).

filter_funs_docs(Doctest, AllDocs) ->
    case Doctest#doctest.funs of
        all ->
            AllDocs;
        Funs when is_list(Funs) ->
            maps:with(Funs, AllDocs)
    end.

% TODO: Pretty print.
output(Doctest, SrcMod, SrcFile, Asserts) ->
    ok = make_dir(Doctest#doctest.location),
    Filename = filename(Doctest#doctest.location, Doctest#doctest.module),
    Content = module_content(SrcMod, SrcFile, Doctest#doctest.module, Asserts),
    file:write_file(Filename, Content).

%% ---------------------------------------------------------------------
%% Normalize

doctest(Attrs, SrcMod, SrcFile) ->
    parse_to_doctest(Attrs, SrcFile, default_doctest(SrcMod)).

parse_to_doctest([Enabled|T], SrcFile, DT) when is_boolean(Enabled) ->
    parse_to_doctest(T, SrcFile, DT#doctest{enabled = Enabled});
parse_to_doctest([[]|T], SrcFile, DT) ->
    parse_to_doctest(T, SrcFile, DT#doctest{funs = []});
parse_to_doctest([[{_,_}|_]=Funs|T], SrcFile, DT) ->
    parse_to_doctest(T, SrcFile, DT#doctest{funs = Funs});
parse_to_doctest([{abs, Location}|T], SrcFile, DT) when is_list(Location) ->
    parse_to_doctest(T, SrcFile, DT#doctest{location = Location});
parse_to_doctest([{cwd, Location}|T], SrcFile, DT) when is_list(Location) ->
    parse_to_doctest(T, SrcFile, DT#doctest{
        location = cwd_location(Location)
    });
parse_to_doctest([Location|T], SrcFile, DT) when is_list(Location) ->
    parse_to_doctest(T, SrcFile, DT#doctest{
        location = relative_location(SrcFile, Location)
    });
parse_to_doctest([all|T], SrcFile, DT) ->
    parse_to_doctest(T, SrcFile, DT#doctest{funs = all});
parse_to_doctest([Module|T], SrcFile, DT) when is_atom(Module) ->
    parse_to_doctest(T, SrcFile, DT#doctest{module = Module});
parse_to_doctest([Map|T], SrcFile, DT) when is_map(Map) ->
    parse_to_doctest(T, SrcFile, DT#doctest{
        enabled = maps:get(enabled, Map, DT#doctest.enabled),
        location = maps:get(location, Map, DT#doctest.location),
        module = maps:get(module, Map, DT#doctest.module),
        funs = maps:get(funs, Map, DT#doctest.funs)
    });
parse_to_doctest([], _SrcFile, #doctest{} = DT) ->
    DT.

cwd_location(Path) ->
    {ok, Cwd} = file:get_cwd(),
    Location = filename:join(Cwd, Path),
    normalize_location(split_location(Location), []).

relative_location(SrcFile, Path) ->
    Location = filename:join(filename:dirname(SrcFile), Path),
    normalize_location(split_location(Location), []).

split_location(Location) ->
    string:split(Location, "..", all).

% NOTE: This implementation can be buggy.
%       The idea here is to normalize relative paths, e.g:
%       ```erlang
%       -doctest "../../".
%       ```
%       And the CWD be "/foo/bar/baz", the result must be `/foo`.
% TODO: Find a better way to do this.
% FIXME: Handle "./" definition.
normalize_location([_,".."|T], Acc) ->
    normalize_location(T, Acc);
normalize_location([H,[]], []) ->
    [[], _ | Acc] = lists:reverse(string:split(H, "/", all)),
    normalize_location([], Acc);
normalize_location([_,[]], Acc) ->
    normalize_location([], Acc);
normalize_location([H|T], Acc) ->
    normalize_location(T, [H|Acc]);
normalize_location([], Acc) ->
    filename:join(lists:reverse(Acc)).

default_doctest(SrcMod) ->
    #doctest{
        enabled = true,
        module = list_to_atom(atom_to_list(SrcMod) ++ "_DOCTEST"),
        funs = all,
        location = cwd_location("test")
    }.

%% ---------------------------------------------------------------------
%% Output

module_content(SrcMod, SrcFile, TestMod, Asserts) ->
["%%%---------------------------------------------------------------------
%%% Tests generated via doctest.
%%% - module: ", atom_to_list(SrcMod), "
%%% - file: ", SrcFile, "
%%%---------------------------------------------------------------------
-module(", atom_to_list(TestMod), ").

-ifdef(TEST).
-include_lib(\"eunit/include/eunit.hrl\").

", funs_tests(SrcMod, Asserts), "
-endif.
"].

filename(Location, Module) ->
    filename:join(Location, atom_to_list(Module) ++ ".erl").

make_dir(Dir) ->
  ok = filelib:ensure_dir(Dir),
  case file:make_dir(Dir) of
    ok -> ok;
    {error, eexist} -> ok
  end.

docs_asserts(Docs) ->
    maps:filtermap(fun(_, Doc) ->
        case capture(Doc) of
            {match, Captured} ->
                case rev_normalize(join(split(Captured)), []) of
                    [] ->
                        false;
                    Norm ->
                        {true, Norm}
                end;
            nomatch ->
                false
        end
    end, Docs).

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

% FIXME: Only exported functions can be tested.
funs_tests(_SrcMod, Asserts) ->
    lists:join("\n", maps:fold(fun({F, A}, Tests, Acc) ->
        % TODO: Check why function_exported always returns false.
        % case erlang:function_exported(SrcMod, F, A) of
            % true ->
                [fun_asserts(F, A, Tests) | Acc]
            % false ->
                % Acc
        % end
    end, [], Asserts)).

fun_asserts(F, A, Tests) ->
[atom_to_list(F), $_, integer_to_list(A), "_test() ->
    [
", lists:join(",\n", lists:map(fun({Left, Right}) ->
    ["        ?assertEqual(", Left, ", ", Right, ")"]
end, Tests)), "
    ].
"].

%% ---------------------------------------------------------------------
%% Forms

filtermap_forms(Types, Filtermap, Forms) when
    is_list(Types), is_function(Filtermap, 1), is_list(Forms) ->
    lists:filtermap(fun(Form) ->
        Type = erl_syntax:type(Form),
        case lists:member(Type, Types) of
            true ->
                Filtermap({Type, Form});
            false ->
                false
        end
    end, Forms).

attributes(Name, Forms) ->
    filtermap_forms([attribute], fun({attribute, Attr}) ->
        case attribute_name(Attr) =:= Name of
            true ->
                {true, normalize_attribute(Attr)};
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
        erl_syntax:function_arity(Fun)
    }.

src_module(Forms) ->
    hd(attributes(module, Forms)).

src_filename(Forms) ->
    [File, _Loc] = hd(attributes(file, Forms)),
    File.

doctest_attrs(Forms) ->
    attributes(doctest, Forms).

doc_attrs(Forms) ->
    do_doc_attrs(filtermap_forms([attribute, function], fun
        ({attribute, Attr}) ->
            case attribute_name(Attr) =:= doc of
                true ->
                    {true, {doc, normalize_attribute(Attr)}};
                false ->
                    false
            end;
        ({function, Fun}) ->
            {true, {function, normalize_function(Fun)}}
    end, Forms), []).

do_doc_attrs([{doc, Doc},{function, {F, A}}|T], Acc) ->
    do_doc_attrs(T, [{{F, A}, Doc} | Acc]);
do_doc_attrs([{function, _}|T], Acc) ->
    do_doc_attrs(T, Acc);
do_doc_attrs([], Acc) ->
    maps:from_list(Acc).
