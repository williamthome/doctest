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
-module(doctest_forms).
-moduledoc false.

% API functions
-export([ module/1
        , filename/1
        , lookup_attribute/2
        , get_attributes/2
        , prepend/2
        , append/2
        ]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

module(Forms) ->
    {ok, {_Ln, Mod}} = lookup_attribute(module, Forms),
    Mod.

filename(Forms) ->
    {ok, {_Ln, Filename}} = lookup_attribute(file, Forms),
    Filename.

lookup_attribute(Name, Forms) ->
    do_lookup_attribute(Forms, Name).

get_attributes(Name, Forms) ->
    lists:flatten(filtermap(
        fun(Type) -> Type =:= attribute end,
        fun({attribute, Attr}) ->
            case attribute_name(Attr) =:= Name of
                true ->
                    {true, normalize_attributes(Attr)};
                false ->
                    false
            end
        end,
        Forms
    )).

prepend(NewForms, Forms) when is_list(NewForms), is_list(Forms) ->
    do_prepend(Forms, NewForms);
prepend(NewForm, Forms) when is_tuple(NewForm) ->
    prepend([NewForm], Forms).

append(NewForms, Forms) when is_list(NewForms), is_list(Forms) ->
    do_append(Forms, NewForms);
append(NewForm, Forms) when is_tuple(NewForm) ->
    append([NewForm], Forms).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_lookup_attribute([Form | Forms], Name) ->
    case is_attribute(Form) andalso attribute_name(Form) =:= Name of
        true ->
            {ok, hd(normalize_attributes(Form))};
        false ->
            do_lookup_attribute(Forms, Name)
    end;
do_lookup_attribute([], _Name) ->
    error.

filtermap(Validate, Filtermap, Forms) when
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

is_attribute(Form) ->
    erl_syntax:type(Form) =:= attribute.

attribute_name(Form) ->
    erl_syntax:atom_value(erl_syntax:attribute_name(Form)).

normalize_attributes(Form) ->
    Exprs = erl_syntax:revert_forms(erl_syntax:attribute_arguments(Form)),
    lists:map(fun(Expr) ->
        {value, Value, []} = erl_eval:expr(Expr, []),
        {line(Expr), Value}
    end, Exprs).

line(Form) ->
    erl_anno:line(erl_syntax:get_pos(Form)).

do_prepend([Form | Forms], NewForms) ->
    case erl_syntax:type(Form) of
        function ->
            NewForms ++ [Form | Forms];
        _ ->
            [Form | do_prepend(Forms, NewForms)]
    end.

do_append([Form | Forms], NewForms) ->
    case erl_syntax:type(Form) of
        eof_marker ->
            NewForms ++ [Form | Forms];
        _ ->
            [Form | do_append(Forms, NewForms)]
    end.
