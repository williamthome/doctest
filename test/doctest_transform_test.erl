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
-module(doctest_transform_test).
-include("src/doctest_check.hrl").

-if(?IS_DOC_ATTRS_SUPPORTED).
-moduledoc """
Module doc tags can also be tested.

```erlang
1> doctest_transform_test:sum(1, 1) =:= 2.
true
```
""".
-endif.

-export([sum/2, mult/2, nodoc/0, nocodeblock/0, notestcodeblock/0, concat/2]).

-ifdef(TEST).
-include("doctest.hrl").
-doctest [sum/2, mult/2, nodoc/0, nocodeblock/0, notestcodeblock/0, concat/2].
-endif.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
```erlang
1> doctest_transform_test:sum(1, 1).
2
2> doctest_transform_test:sum(1,
.. 2).
3
```
""".
-endif.
sum(A, B) ->
    A + B.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
```erlang
1> doctest_transform_test:mult(1, 1).
1
2> doctest_transform_test:mult(1,
.. 2).
2
```
""".
-endif.
mult(A, B) ->
    A * B.

nodoc() ->
    ok.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
""".
-endif.
nocodeblock() ->
    ok.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
```erlang
foo() ->
    bar.
```
""".
-endif.
notestcodeblock() ->
    ok.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
```erlang
1> Foo = "foo".
"foo"
2> doctest_transform_test:concat(
.. Foo,
..   "bar"
.. ).
"foobar"
```
""".
-endif.
concat(A, B) ->
    A ++ B.
