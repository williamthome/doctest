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

% API functions
-export([parse_transform/2]).

% Support functions
-export([ attributes/2
        , filtermap_forms/3
        , attribute_name/1
        , normalize_attribute/1
        , form_line/1
        ]).

% Contains the settings record.
-include("doctest_parse_transform.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opt) ->
    % Extract and run tests
    Extractor = doctest_markdown,
    ensure_modules_loaded(Extractor),
    Settings = settings(doctest_attrs(Forms), default_settings()),
    Tests = Extractor:extract_forms_tests(Forms, Settings),
    doctest_eunit:test(Tests, Settings#settings.eunit),
    % Return the original forms
	Forms.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

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
        {form_line(Expr), Value}
    end, Exprs) of
        [H] ->
            H;
        List ->
            List
    end.

form_line(Form) ->
    erl_anno:line(erl_syntax:get_pos(Form)).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

ensure_modules_loaded(Extractor) ->
    [code:ensure_loaded(Mod) || Mod <- [
        doctest_extract,
        Extractor,
        doctest_eunit
    ]].

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

default_settings() ->
    Env = application:get_all_env(doctest),
    #settings{
        enabled = proplists:get_value(enabled, Env, true),
        moduledoc = proplists:get_value(moduledoc, Env, true),
        funs = proplists:get_value(funs, Env, true),
        eunit = proplists:get_value(eunit, Env, resolve)
    }.

doctest_attrs(Forms) ->
    [Attr || {_Ln, Attr} <- attributes(doctest, Forms)].
