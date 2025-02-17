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
-module(doctest_test).
-export([foo/0, bar/0, bindings/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{
        bindings => #{'Foo' => foo},
        extractors => [doctest_extract_attr, doctest_extract_tag]
    }).
-endif.

-define(IS_DOC_ATTRS_SUPPORTED, ?OTP_RELEASE >= 27).

-if(?IS_DOC_ATTRS_SUPPORTED).
-moduledoc """
Module doc tags can also be tested.

```erlang
1> doctest_test:foo()
.. =:= bar.
false
2>                           % Are comments allowed? Yes! But only in
.. doctest_test:foo() % expressions, not at the last line, and
.. =:=                       % not in results, like the lines below.
.. foo.
true
```
""".
-endif.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
Test #1
```erlang
1> % Test non-exported functions is allowed.
.. foo().
foo
```
Test #2
```erlang
1> doctest_test:foo() =:= foo.
true
```
""".
-endif.
foo() ->
    do_foo().

%% @doc This function is private and should be skipped.
%% ```
%% 1> do_foo().
%% foo
%% '''

do_foo() ->
    foo.

%% @doc Comments are also supported.
%% ```
%% 1> Bar = doctest_test:bar().
%% bar
%% 2> Bar =:= bar.
%% true
%% '''

bar() ->
    bar.

-if(?IS_DOC_ATTRS_SUPPORTED).
-doc """
For example, #{bindings => #{'Foo' => foo}}

```
> doctest_test:bindings(Foo).
foo
```
""".
-endif.
bindings(Foo) ->
    Foo.
