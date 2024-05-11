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
Generate tests for doc attributes via parse transform.

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
-record(settings, {
    enabled = true,
    moduledoc = true,
    funs = true,
    eunit = resolve
}).

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opt) ->
    % Parse docs and run tests
    Settings = settings(doctest_attrs(Forms), #settings{}),
    DocAttrs = filter_doc_attrs(Settings, doc_attrs(Forms)),
    doctest_eunit:test(tests(Forms, DocAttrs), Settings#settings.eunit),
    % Return the original forms
	Forms.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

settings([Enabled | T], Settings) when is_boolean(Enabled) ->
    settings(T, Settings#settings{enabled = Enabled});
settings([Funs | T], Settings) when is_list(Funs) ->
    settings(T, Settings#settings{funs = Funs});
settings([{enabled, Enabled} | T], Settings) when is_boolean(Enabled) ->
    settings(T, Settings#settings{enabled = Enabled});
settings([{moduledoc, Enabled} | T], Settings) when is_boolean(Enabled) ->
    settings(T, Settings#settings{moduledoc = Enabled});
settings([{funs, Enabled} | T], Settings) when is_boolean(Enabled)->
    settings(T, Settings#settings{funs = Enabled});
settings([{funs, Funs} | T], Settings) when is_list(Funs) ->
    settings(T, Settings#settings{funs = Funs});
settings([{eunit, Eunit} | T], Settings) ->
    settings(T, Settings#settings{eunit = Eunit});
settings([Map | T], Settings) when is_map(Map) ->
    settings(T, Settings#settings{
        enabled = maps:get(enabled, Map, Settings#settings.enabled),
        moduledoc = maps:get(moduledoc, Map, Settings#settings.moduledoc),
        funs = maps:get(funs, Map, Settings#settings.funs),
        eunit = maps:get(eunit, Map, Settings#settings.eunit)
    });
settings([], #settings{} = Settings) ->
    Settings.

filter_doc_attrs(#settings{moduledoc = ShouldTestModDoc, funs = FunsOpts}, AllDocs) ->
    lists:filter(fun
        ({moduledoc, _Doc}) ->
            ShouldTestModDoc;
        ({{doc, {function, {F, A, _Ln}}}, _Doc}) ->
            keep_fun({F, A}, FunsOpts)
    end, AllDocs).

keep_fun(_Fun, true) ->
    true;
keep_fun(_Fun, false) ->
    false;
keep_fun(Fun, Funs) when is_list(Funs) ->
    lists:member(Fun, Funs).

doctest_attrs(Forms) ->
    [Attr || {_Ln, Attr} <- attributes(doctest, Forms)].

doc_attrs(Forms) ->
    normalize_doc_attrs(filtermap_forms(
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

normalize_doc_attrs([{moduledoc, Doc} | T], Acc) ->
    normalize_doc_attrs(T, [{moduledoc, Doc} | Acc]);
normalize_doc_attrs([{doc, Doc},{function, Fun} | T], Acc) ->
    normalize_doc_attrs(T, [{{doc, {function, Fun}}, Doc} | Acc]);
normalize_doc_attrs([{function, _} | T], Acc) ->
    normalize_doc_attrs(T, Acc);
normalize_doc_attrs([], Acc) ->
    Acc.

tests(Forms, DocAttrs) ->
    File = file(Forms),
    lists:foldl(fun({Kind, {MarkdownLn, Markdown}}, Acc) ->
        case doctest_md:code_blocks(Markdown) of
            {ok, CodeBlocks} ->
                {ok, M, Bin} = compile:forms(Forms, [
                    {i, "eunit/include/eunit.hrl"}
                ]),
                {module, M} = code:load_binary(M, File, Bin),
                case Kind of
                    moduledoc ->
                        [doctest_eunit:moduledoc_tests(M, MarkdownLn, CodeBlocks)
                        | Acc];
                    {doc, {function, {F, A, Ln}}} ->
                        [doctest_eunit:doc_tests({M, F, A}, Ln, CodeBlocks)
                        | Acc]
                end;
            none ->
                Acc
        end
    end, [], DocAttrs).

file(Forms) ->
    [{_Ln, File}, _Loc] = hd(attributes(file, Forms)),
    File.

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
