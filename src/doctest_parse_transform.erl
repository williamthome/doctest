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
-record(doctest, {enabled, moduledoc, funs}).

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opt) ->
    % Parse docs and run tests
    File = file(Forms),
    Docs = docs(doctest(doctest_attrs(Forms), File), doc_attrs(Forms)),
    doctest:run(tests(File, Forms, Docs)),
    % Return the original forms
	Forms.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

docs(#doctest{moduledoc = ModDoc, funs = Funs}, AllDocs) ->
    lists:filter(fun
        ({moduledoc, _Doc}) ->
            ModDoc;
        ({{doc, {function, {F, A, _Ln}}}, _Doc}) ->
            doctest:should_test_function(Funs, {F, A})
    end, AllDocs).

doctest(Attrs, SrcFile) ->
    parse_to_doctest(Attrs, SrcFile, #doctest{
        enabled = true,
        moduledoc = true,
        funs = true
    }).

parse_to_doctest([Enabled | T], SrcFile, DocTest) when is_boolean(Enabled) ->
    parse_to_doctest(T, SrcFile, DocTest#doctest{enabled = Enabled});
parse_to_doctest([Funs | T], SrcFile, DocTest) when is_list(Funs) ->
    parse_to_doctest(T, SrcFile, DocTest#doctest{funs = Funs});
parse_to_doctest([{enabled, Enabled} | T], SrcFile, DocTest) when is_boolean(Enabled) ->
    parse_to_doctest(T, SrcFile, DocTest#doctest{enabled = Enabled});
parse_to_doctest([{moduledoc, Enabled} | T], SrcFile, DocTest) when is_boolean(Enabled) ->
    parse_to_doctest(T, SrcFile, DocTest#doctest{moduledoc = Enabled});
parse_to_doctest([{funs, Enabled} | T], SrcFile, DocTest) when is_boolean(Enabled)->
    parse_to_doctest(T, SrcFile, DocTest#doctest{funs = Enabled});
parse_to_doctest([{funs, Funs} | T], SrcFile, DocTest) when is_list(Funs) ->
    parse_to_doctest(T, SrcFile, DocTest#doctest{funs = Funs});
parse_to_doctest([Map | T], SrcFile, DocTest) when is_map(Map) ->
    parse_to_doctest(T, SrcFile, DocTest#doctest{
        enabled = maps:get(enabled, Map, DocTest#doctest.enabled),
        moduledoc = maps:get(moduledoc, Map, DocTest#doctest.moduledoc),
        funs = maps:get(funs, Map, DocTest#doctest.funs)
    });
parse_to_doctest([], _SrcFile, #doctest{} = DocTest) ->
    DocTest.

tests(File, Forms, Docs) ->
    lists:foldl(fun({Kind, {MdLn, Md}}, Acc) ->
        case doctest:code_block(Md) of
            {ok, CodeBlock} ->
                {ok, M, Bin} = compile:forms(Forms, [
                    {i, "eunit/include/eunit.hrl"}
                ]),
                {module, M} = code:load_binary(M, File, Bin),
                Chunk = doctest:chunk(CodeBlock),
                Tests = case Kind of
                    moduledoc ->
                        doctest:parse_mod(M, MdLn, Chunk);
                    {doc, {function, {F, A, Ln}}} ->
                        doctest:parse_fun({M, F, A}, Ln, Chunk)
                end,
                [Tests | Acc];
            none ->
                Acc
        end
    end, [], Docs).

file(Forms) ->
    [{_Ln, File}, _Loc] = hd(attributes(file, Forms)),
    File.

doctest_attrs(Forms) ->
    [Attr || {_Ln, Attr} <- attributes(doctest, Forms)].

doc_attrs(Forms) ->
    do_doc_attrs(filtermap_forms(
        fun(Type) -> lists:member(Type, [attribute, function]) end,
        fun
            ({attribute, Attr}) ->
                case attribute_name(Attr) of
                    moduledoc ->
                        case normalize_attribute(Attr) of
                            {Ln, Md} when is_list(Md); is_binary(Md) ->
                                {true, {moduledoc, {Ln, Md}}};
                            _ ->
                                false
                        end;
                    doc ->
                        case normalize_attribute(Attr) of
                            {Ln, Md} when is_list(Md); is_binary(Md) ->
                                {true, {doc, {Ln, Md}}};
                            _ ->
                                false
                        end;
                    _ ->
                        false
                end;
            ({function, Fun}) ->
                {true, {function, normalize_function(Fun)}}
        end,
        Forms
    ), []).

do_doc_attrs([{moduledoc, Doc} | T], Acc) ->
    do_doc_attrs(T, [{moduledoc, Doc} | Acc]);
do_doc_attrs([{doc, Doc},{function, Fun} | T], Acc) ->
    do_doc_attrs(T, [{{doc, {function, Fun}}, Doc} | Acc]);
do_doc_attrs([{function, _} | T], Acc) ->
    do_doc_attrs(T, Acc);
do_doc_attrs([], Acc) ->
    Acc.

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
        {form_ln(Expr), Value}
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
        form_ln(Fun)
    }.

form_ln(Form) ->
    erl_anno:line(erl_syntax:get_pos(Form)).
