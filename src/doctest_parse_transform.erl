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

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opts) ->
    spawn(fun() ->
        code:ensure_loaded(doctest),
        doctest:forms(Forms, parse_opts(doctest_attrs(Forms), #{}))
    end),
	Forms.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

parse_opts([Enabled | T], Opts) when is_boolean(Enabled) ->
    parse_opts(T, Opts#{enabled => Enabled});
parse_opts([Funs | T], Opts) when is_list(Funs) ->
    parse_opts(T, Opts#{funs => Funs});
parse_opts([{enabled, Enabled} | T], Opts) when is_boolean(Enabled) ->
    parse_opts(T, Opts#{enabled => Enabled});
parse_opts([{moduledoc, Enabled} | T], Opts) when is_boolean(Enabled) ->
    parse_opts(T, Opts#{moduledoc => Enabled});
parse_opts([{funs, Enabled} | T], Opts) when is_boolean(Enabled) ->
    parse_opts(T, Opts#{funs => Enabled});
parse_opts([{funs, Funs} | T], Opts) when is_list(Funs) ->
    parse_opts(T, Opts#{funs => Funs});
parse_opts([{eunit, Eunit} | T], Opts) ->
    parse_opts(T, Opts#{eunit => Eunit});
parse_opts([{extractors, Extractors} | T], Opts) when is_list(Extractors) ->
    parse_opts(T, Opts#{extractors => Extractors});
parse_opts([Map | T], Opts) when is_map(Map) ->
    parse_opts(T, maps:merge(Opts, Map));
parse_opts([], #{} = Opts) ->
    Opts.

doctest_attrs(Forms) ->
    [Attr || {_Ln, Attr} <- attributes(doctest, Forms)].

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
        {ln(Expr), Value}
    end, Exprs) of
        [H] ->
            H;
        List ->
            List
    end.

ln(Form) ->
    erl_anno:line(erl_syntax:get_pos(Form)).
