%%% ---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @doc Generate tests for doc attributes via parse transform.
%%%
%%% Just plug the header on your module:
%%% ```
%%% -ifdef(TEST).
%%% -include_lib("doctest/include/doctest.hrl").
%%% -doctest <options> % optional
%%% -endif.
%%% '''
%%%
%%% @author William Fank Thomé [https://github.com/williamthome]
%%% @end
%%% ---------------------------------------------------------------------
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
-module(doctest_transform).

% API functions
-export([parse_transform/2]).

%%%=====================================================================
%%% API functions
%%%=====================================================================

parse_transform(Forms, _Opts) ->
	doctest_forms:append(doctest_fun(Forms),
        doctest_forms:prepend(doctest_export(), Forms)).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

doctest_export() ->
    merl:quote("-export([doctest_test/0]).").

doctest_fun(Forms) ->
    erl_syntax:revert(merl:qquote([
        "doctest_test() ->",
        "    ok =:= doctest:module('@module', _@options)."
    ], [
        {module, merl:term(doctest_forms:module(Forms))},
        {options, merl:term(parse_opts(doctest_attrs(Forms), #{}))}
    ])).

doctest_attrs(Forms) ->
    [Attr || {_Ln, Attr} <- doctest_forms:get_attributes(doctest, Forms)].

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
parse_opts([{eunit_opts, EunitOpts} | T], Opts) ->
    parse_opts(T, Opts#{eunit_opts => EunitOpts});
parse_opts([{extractors, Extractors} | T], Opts) when is_list(Extractors) ->
    parse_opts(T, Opts#{extractors => Extractors});
parse_opts([Map | T], Opts) when is_map(Map) ->
    parse_opts(T, maps:merge(Opts, Map));
parse_opts([], #{} = Opts) ->
    Opts.
