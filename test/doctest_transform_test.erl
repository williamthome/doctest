%%% @doc Module doc can also be tested.
%%% ```
%%% 1> doctest_transform_test:sum(1, 1) =:= 2.
%%% true
%%% '''
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
-module(doctest_transform_test).
-export([skip_match/1, sum/2, mult/2, nodoc/0, nocodeblock/0, notestcodeblock/0]).

-ifdef(TEST).
-include("doctest.hrl").
-doctest #{
    extractors => [doctest_extract_attr, doctest_extract_tag],
    doc => [skip_match/1, sum/2, mult/2, nodoc/0, nocodeblock/0, notestcodeblock/0]
}.
-endif.

-include("src/doctest_check.hrl").

-if(?IS_DOC_ATTRS_SUPPORTED).

-doc """
Skip with underscore.

```
> Foo = foo.
_
> doctest_transform_test:skip_match(
.   Foo
. ).
foo
```

Skip when no right side.

```
> Foo = foo.
> doctest_transform_test:skip_match(
    Foo
  ).
foo
```
""".
skip_match(Foo) ->
    Foo.

-doc """
```erlang
1> doctest_transform_test:sum(1, 1).
2
2> doctest_transform_test:sum(1,
.. 2).
3
```
""".
sum(A, B) ->
    A + B.

-doc """
```erlang
1> One = 1.
1
2> doctest_transform_test:mult(One, One).
1
3> doctest_transform_test:mult(One,
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

-else.

%% @doc
%% Skip with underscore.
%%
%% ```
%% > Foo = foo.
%% _
%% > doctest_transform_test:skip_match(
%% .   Foo
%% . ).
%% foo
%% '''
%%
%% Skip when no right side.
%%
%% ```
%% > Foo = foo.
%% > doctest_transform_test:skip_match(
%%     Foo
%%   ).
%% foo
%% '''
skip_match(Foo) ->
    Foo.

%% @doc
%% ```
%% 1> doctest_transform_test:sum(1, 1).
%% 2
%% 2> doctest_transform_test:sum(1,
%% .. 2).
%% 3
%% '''
sum(A, B) ->
    A + B.

%% @doc
%% ```
%% 1> doctest_transform_test:mult(1, 1).
%% 1
%% 2> doctest_transform_test:mult(1,
%% .. 2).
%% 2
%% '''
mult(A, B) ->
    A * B.

nodoc() ->
    ok.

%% @doc
%% '''
nocodeblock() ->
    ok.

%% @doc
%% ```
%% foo() ->
%%     bar.
%% '''
notestcodeblock() ->
    ok.

-endif.
