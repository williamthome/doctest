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
-module(doctest_foo).
-author("William Fank Thomé [https://github.com/williamthome]").
-compile([export_all, nowarn_export_all]).

-ifdef(TEST).
-include("doctest.hrl").
-endif.

-doc """
```erlang
1> doctest_foo:sum(1, 1).
2
2> doctest_foo:sum(1,
.. 2).
3
```
""".
sum(A, B) ->
    A + B.

-doc """
```erlang
1> doctest_foo:mult(1, 1).
1
2> doctest_foo:mult(1,
.. 2).
2
```
""".
mult(A, B) ->
    A * B.

nodoc() ->
    ok.

-doc """
""".
nocodeblock() ->
    ok.

-doc """
```erlang
foo() ->
    bar.
```
""".
notestcodeblock() ->
    ok.

-doc """
```erlang
1> doctest_foo:concat(
.. "foo",
..   "bar"
.. ).
"foobar"
```
""".
concat(A, B) ->
    A ++ B.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

output_test() ->
    {ok, Cwd} = file:get_cwd(),
    Filename = filename:join([Cwd, "test", ?MODULE_STRING ++ "_DOCTEST.erl"]),
    {ok, Content} = file:read_file(Filename),
    ?assertEqual(<<
        "%%%---------------------------------------------------------------------\n"
        "%%% Tests generated via doctest.\n"
        "%%% - module: " ?MODULE_STRING "\n"
        "%%% - file: " ?FILE "\n"
        "%%%---------------------------------------------------------------------\n"
        "-module(doctest_foo_DOCTEST).\n"
        "\n"
        "-ifdef(TEST).\n"
        "-include_lib(\"eunit/include/eunit.hrl\").\n"
        "\n"
        "sum_2_test() ->\n"
        "    [\n"
        "        ?assertEqual(doctest_foo:sum(1, 1), 2),\n"
        "        ?assertEqual(doctest_foo:sum(1, 2), 3)\n"
        "    ].\n"
        "\n"
        "mult_2_test() ->\n"
        "    [\n"
        "        ?assertEqual(doctest_foo:mult(1, 1), 1),\n"
        "        ?assertEqual(doctest_foo:mult(1, 2), 2)\n"
        "    ].\n"
        "\n"
        "concat_2_test() ->\n"
        "    [\n"
        "        ?assertEqual(doctest_foo:concat( \"foo\", \"bar\" ), \"foobar\")\n"
        "    ].\n"
        "\n"
        "-endif.\n"
        ""
    >>, Content).

-endif.
