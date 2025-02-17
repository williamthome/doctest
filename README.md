# doctest

An Erlang library to test `@doc` tags and `-moduledoc` and `-doc` attributes.

It is compatible with the [doctests syntax introduced in OTP-28](https://github.com/erlang/otp/pull/9315).

> [!NOTE]
>
> The `-moduledoc` and `-doc` attributes were introduced in [OTP 27](https://www.erlang.org/docs/27/system/documentation).

## Installation

```erlang
% rebar.config
% {minimum_otp_vsn, "24"}.
{profiles, [
    {test, [
        % 'debug_info' is required to extract doc chunks.
        {erl_opts, [debug_info]},
        {deps, [{doctest, "~> 0.11"}]}
    ]}
]}.
% 'doctest_eunit_report' is required to pretty print and correctly displays the failed Eunit tests.
{eunit_opts, [no_tty, {report, {doctest_eunit_report, []}}]}.
```

> [!IMPORTANT]
>
> `doctest` won't run any test when `cover` is enabled, for example:
>
> ```erlang
> {profiles, [
>     {test, [
>         {cover_enabled, true},
>         % ...
>     ]}
> ]}.
> ```
>
> There is an open [PR](https://github.com/erlang/otp/pull/9433) to fix this bug in the OTP repository.

## Overview

Erlang documentation can be written:

- Via [EDoc](https://www.erlang.org/doc/apps/edoc/chapter) by using the `@doc` tag, e.g.:

  ````erlang
  %% @doc Prints "Hello, Joe!".
  %%
  %% Example:
  %%
  %% ```
  %% > print().
  %% "Hello, Joe!"
  %% '''
  print() -> "Hello, Joe!".
  ````

- Or via [ExDoc](https://hexdocs.pm/ex_doc/readme.html), by using the `-moduledoc` and
`-doc` attributes [introduced in OTP 27](https://www.erlang.org/doc/system/documentation), e.g.:

  ````erlang
  -doc """
  Prints "Hello, Joe!".

  Example:

  ```
  > print().
  "Hello, Joe!"
  ```
  """.
  print() -> "Hello, Joe!".
  ````

There are some rules to test documentation. One rule is that only code blocks
are testable. Via `EDoc/tags`, code blocks are code between ` ``` ` and `'''`
(triple backticks and triple single quotes), and via `ExDoc/attributes`,
they are code between ` ``` ` and ` ``` ` (triple quotes and triple quotes).
The code of the code blocks follows the same rules as the current Erlang shell, for example:

```erlang
> % - Comments and multiline expressions are allowed;
  % - Multiline expressions must be aligned;
  % - Invalid syntaxes are skipped.
  print().
"Hello, Joe!"
> % All tests compare the equality between the expression and the result.
  % The example below is translated to an `?assertEqual` macro result:
  % > ?assertEqual(true, print() =/= "Hello, World").
  print() =/= "Hello, World!".
true
```

### Optionally

1. Variable outputs can be skipped by replacing the right side (result) by an
   underscore (`_`) or without giving a result to it by starting a new expression
2. The `erlang` language in code blocks is not required
3. The syntax can contains the line numbers and dots between multiple lines,
   like in the erlang shell

For example:

````erlang
-doc """
```erlang
1> A =
.. 1.
_
2> B = 1. % It's not required to show the result
3> A + B.
2
```
""".
foo(Foo) -> Foo.
````

## Usage

````erlang
-module(mymodule).
-export([sum/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{
        % Default options:

        % Enable or turn off module doc tests.
        % Spec: boolean()
        moduledoc => true,

        % Enable or turn off functions doc tests or define a list of functions to be tested.
        % Spec: boolean() | [{atom(), arity()}].
        doc => true,

        % Set the EUnit options. 'rebar3_config' tries to resolve the options defined in the rebar3.
        % Spec: rebar3_config | [term()]
        eunit_opts => rebar3_config,

        % Overrides the code blocks extractors. See the 'doctest_extract' behavior.
        % Custom extractors are allowed.
        % Spec: [module()]
        extractors => [doctest_extract_attr], % OTP < 27 => [doctest_extract_tag]

        % Bind value to variables. Could be a proplist or a map.
        % Spec: erl_eval:binding_struct()
        bindings => #{}
    }).
-endif.

% The `sum/2` will be tested by default because it contains a valid Markdown
% code block. Multiple Markdown code blocks can be defined.
-doc """
```
> mymodule:sum(1, 1).
2
```
""".
sum(A, B) ->
    A + B.
````

> [!NOTE]
>
> Please see the [rebar documentation](https://rebar3.org/docs/testing/eunit/#eunit_opts)
> for more information about the EUnit options.

## Example

> [!IMPORTANT]
>
> If the OTP version is below 27, please only consider the `@doc` tags inside comments as a valid code.
> The `-moduledoc` and `-doc` attributes are valid if the OTP version is equal to or above 27.

Take this module:

````erlang
 1 │ -module(greeting).
 2 │ -moduledoc """
 3 │ Module documentation are testable.
 4 │
 5 │ ```
 6 │ > greeting:print() =:= "Hello, Joe!".
 7 │ true
 8 │ ```
 9 │ """.
10 │
11 │ -export([print/0]).
12 │
13 │ -ifdef(TEST).
14 │ -include_lib("eunit/include/eunit.hrl").
15 │  doctest_test() ->
16 │      doctest:module(?MODULE).
17 │ -endif.
18 │
19 │ -doc """
20 │ ```
21 │ > greeting:print().
22 │ "Hello, World!"
23 │ ```
24 │ """.
25 │ print() ->
26 │     "Hello, Joe!".
````

Running the EUnit test via rebar3:

```bash
$ rebar3 eunit --module greeting
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling doctest
===> Performing EUnit tests...
 PASS  ./src/greeting.erl:6 -moduledoc
 FAIL  ./src/greeting.erl:21 -doc

 ❌ assertEqual

    │
 21 │ > greeting:print().
 22 │ "Hello, World!"
    │
    └── at ./src/greeting.erl:21

Expected:
"Hello, World!"

Received:
"Hello, Joe!"



Tests: 1 failed, 1 passed, 2 total
 Time: 0.003 seconds




Tests: 1 passed, 1 total
 Time: 0.0 seconds
```

> [!NOTE]
>
> The output above is by using the `doctest_eunit_report` as the EUnit report.

It is fine to put the `doctest_test` function in a Common Test module, for example:

```erlang
-module(greeting_SUITE).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 doctest_test() ->
     doctest:module(greeting).
-endif.
```

> [!NOTE]
>
> The `doctest_test` function name could be any name of your choice.

## Doctest EUnit Reporter

There is a built-in EUnit reporter called `doctest_eunit_report` to display the
tests results correctly. Set it in the EUnit options of the project options, e.g.:

```erlang
% rebar3.config
{eunit_opts, [
    no_tty,
    {report, {doctest_eunit_report, [
        % Default options

        % Change the default depth of the output result.
        % Spec: pos_integer()
        {print_depth, 15}
    ]}}
]}.
```

An example of the `doctest_eunit_report` output:
![doctest_eunit_report](/assets/reporter-go-to-definition.gif)

## Sponsors

If you like this tool, please consider [sponsoring me](https://github.com/sponsors/williamthome).
I'm thankful for your never-ending support :heart:

I also accept coffees :coffee:

[!["Buy Me A Coffee"](https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png)](https://www.buymeacoffee.com/williamthome)

## Contributing

### Issues

Feel free to [submit an issue on Github](https://github.com/williamthome/doctest/issues/new).

## License

Copyright (c) 2024 [William Fank Thomé](https://github.com/williamthome)

`doctest` is 100% open source and community-driven. All components are available under the Apache 2 License on [GitHub](https://github.com/williamthome/doctest).

See [LICENSE.md](LICENSE.md) for more information.
