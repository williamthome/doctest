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
-module(doctest_test_module).
-include("src/doctest_check.hrl").

-if(?IS_DOC_ATTRS_SUPPORTED).
-moduledoc """
Module doc tags can also be tested.

```erlang
1> doctest_test_module:foo()
.. =:= bar.
false
2>                           % Are comments allowed? Yes! But only in
.. doctest_test_module:foo() % expressions, not at the last line, and
.. =:=                       % not in results, like the lines below.
.. foo.
true
```
""".
-endif.

-export([foo/0, bar/0]).

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
Foo
```erlang
1> % Test non-exported functions is allowed.
.. foo().
foo
```
Bar
```erlang
1> doctest_test_module:foo() =:= foo.
true
```
""".
-endif.
foo() ->
    foo.

%% @doc Comments are also supported.
%% ```
%% 1> Bar = doctest_test_module:bar().
%% bar
%% 2> Bar =:= bar.
%% true
%% '''

bar() ->
    bar.
