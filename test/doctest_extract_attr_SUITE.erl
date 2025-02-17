%%%---------------------------------------------------------------------
%%% Copyright 2025 William Fank Thomé
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
-module(doctest_extract_attr_SUITE).
-compile([export_all, nowarn_export_all]).

-if(?OTP_RELEASE >= 27).
-moduledoc """
Text before.

```
> hello =/= world.
true
```

Text after.

```erlang
1> hello =/= world.
true
```
""".

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{
        bindings => #{'M' => ?MODULE},
        extractors => [doctest_extract_attr]
    }).
-endif.

-doc """
```
> M:single_line_test().
ok
```
""".
single_line_test() ->
    ok.

-doc """
```
> M:multi_line_test().
[
    a,
    b,
    b
]
> M:multi_line_test().
[
    a,
    b,
    b
]
```

```
> M:multi_line_test()
  =/= [].
true
```
""".
multi_line_test() ->
    [
        a,
        b,
        b
    ].

-doc """
```
> A = 1.
> B = 1.
> A + B.
2
```

```
> A 
  = 1.
> B 
  = 1.
> A + B.
2
```
""".
skip_line_test() ->
    ok.

-doc """
```
> % before expr
  M:comment_test(). % after expr
ok
```
""".
comment_test() ->
    ok.

-doc """
```erlang
1> % before expr
.. M:shell_format_test(). % after expr
ok
2> A = 1.
3> B = 1.
4> A + B.
2
```
""".
shell_format_test() ->
    ok.
-endif.
