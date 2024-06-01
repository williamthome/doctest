# doctest

An Erlang library to test `@doc` tags and `-moduledoc` and `-doc` attributes.

> **Note**
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
        {deps, [{doctest, "0.9.0"}]}
    ]}
]}.
% 'doctest_eunit_report' is required to pretty print and correctly displays the failed tests.
{eunit_opts, [no_tty, {report, {doctest_eunit_report, []}}]}.
```

## Overview

Erlang documentation can be written:
- Via [EDoc](https://www.erlang.org/doc/apps/edoc/chapter) by using the `@doc` tag, e.g.:
  ```erlang
  %% @doc Prints "Hello, Joe!"
  %% Example:
  %% ```
  %% 1> print().
  %% "Hello, Joe!"
  %% '''
  print() -> "Hello, Joe!".
  ```
- Or via [ExDoc](https://hexdocs.pm/ex_doc/readme.html), by using the `-moduledoc` and `-doc` attributes [introduced in OTP 27](https://www.erlang.org/doc/system/documentation), e.g.:
  ````erlang
  -doc """
  Prints "Hello, Joe!"
  Example:
  ```erlang
  1> print().
  "Hello, Joe!"
  ```
  """.
  print() -> "Hello, Joe!".
  ````

There are some rules to test documentation. One rule is that only code blocks are testable. Via `EDoc/tags`, code blocks are code between ` ``` ` and `'''` (triple backticks and triple single quotes), and via `ExDoc/attributes`, they are code between ` ``` ` and ` ``` ` (triple quotes and triple quotes). The code of the code blocks follows the same rules as the current Erlang shell, for example:
```erlang
1> % - Comments and multiline expressions are allowed;
.. % - Number sequence must be respected, starting from 1 to N;
.. % - Multiline expressions must be aligned;
.. % - Invalid syntaxes are skipped.
.. print().
"Hello, Joe!"
2> % All tests compare the equality between the expression and
.. % the result (left = right). The example below is translated to:
.. % ?assertEqual(true, print() =/= "Hello, World!")
.. print() =/= "Hello, World!".
true
```

## Usage

There are two ways to test your documentation:
- Manually calling `doctest:module/1,2` functions in the Erlang shell, e.g.:
  ```erlang
  1> doctest:module(greeting, #{
     % Options (please see the options below)
  }).
  ```
- Or via parse transformation by using the `doctest_transform` module included in the `doctest/include/doctest.hrl` and then running `rebar3 eunit`, e.g.:
  ```erlang
  -ifdef(TEST).
  % The doctest header exports all functions and sets `doctest_transform` as a parse_transform:
  -include_lib("doctest/include/doctest.hrl").
  -doctest #{
      % Options (please see the options below)
  }.
  -endif.

  % And then running:
  % $ rebar3 eunit
  ```

### Options

The options are passed via a map:
```erlang
#{
    % Enable or turn off any test.
    % Default: true.
    enabled => boolean(), false.

    % Enable or turn off module doc tests.
    % Default: true.
    moduledoc => boolean(),

    % Enable or turn off functions doc tests or define a list of functions to be tested.
    % Default: true.
    doc => boolean() | [{atom(), arity()}],

    % Set the EUnit options. 'rebar3_config' tries to resolve the options defined in the rebar3.
    % Default: rebar3_config.
    eunit_opts => rebar3_config | [term()],

    % Overrides the code blocks extractors. See the 'doctest_extract' behavior. Custom extractors are allowed.
    % Default:
    % - OTP < 27: [doctest_extract_tag];
    % - OTP >= 27: [doctest_extract_attr, doctest_extract_tag].
    extractors => [module()]
}
```

> **Note**
>
> Please see the [rebar documentation](https://rebar3.org/docs/testing/eunit/#eunit_opts) for more information about the EUnit options.

In a module, the `-doctest` attribute is used to override the default settings via a map, e.g., `-doctest #{enabled => true}.`, or via some shortcuts, for example:
- `{enabled, boolean()}` or `boolean()`: equivalent to enabled option.
  ```erlang
  -doctest true.
  ```
- `{moduledoc, boolean()}`: equivalent to moduledoc option.
  ```erlang
  -doctest {moduledoc, true}.
  ```
- `{doc, boolean() | [{atom(), arity()}]}` or `[{atom(), arity()}]`: equivalent to doc option.
  ```erlang
  -doctest [print/0].
  ```
- `{eunit_opts, rebar3_config | term()}`: equivalent to eunit_opts option.
  ```erlang
  -doctest {eunit_opts, rebar3_config}.
  ```
- `{extractors, [module()]}`: equivalent to extractors option.
  ```erlang
  -doctest {extractors, [doctest_extract_attr, doctest_extract_tag]}.
  ```

> **Note**
>
> Multiple `-doctest` attributes are allowed.

#### Global Options

Options can be globally defined via a [config file](https://www.erlang.org/doc/man/config.html), e.g.:
```erlang
% config/sys.config
[{doctest, [
    {enabled, true},
    {moduledoc, true},
    {doc, true},
    {eunit_opts, rebar3_config},
    {extractors, [doctest_extract_attr, doctest_extract_tag]}
]}].
```

Please make sure to add the config file to the rebar3 config, e.g.:
```erlang
{shell, [{config, "config/sys.config"}]}.
{eunit_opts, [{sys_config, ["config/sys.config"]}]}.
```

## Example

> **Important**
>
> If the OTP version is below 27, please only consider the `@doc` tags inside comments as a valid code. The `-moduledoc` and `-doc` attributes are valid if the OTP version is equal to or above 27.

Take this module:
````erlang
 1 │ -module(greeting).
 2 │ -moduledoc """
 3 │ Module documentation are testable.
 4 │
 5 │ ```erlang
 6 │ 1> greeting:print() =:= "Hello, Joe!".
 7 │ true
 8 │ ```
 9 │ """.
10 │
11 │ -export([print/0]).
12 │
13 │ -ifdef(TEST).
14 │ -include_lib("doctest/include/doctest.hrl").
15 │ -endif.
16 │
17 │ -doc """
18 │ ```erlang
19 │ 1> greeting:print().
20 │ "Hello, World!"
21 │ ```
22 │ """.
23 │ print() ->
24 │     hello().
25 │
26 │ %% @doc Non-exported functions are testable.
27 │ %%
28 │ %% ```
29 │ %% 1> % Bound variables to a value is valid, e.g.:
30 │ %% .. Greeting = hello().
31 │ %% "Hello, Joe!"
32 │ %% 2> Greeting =:= "Hello, World!".
33 │ %% true
34 │ %% '''
35 │ hello() ->
36 │     "Hello, Joe!".
````

As mentioned before, there are two ways to run the tests.
- Via `doctest:module/1,2` in the Erlang shell, e.g.:
    ```shell
    $ rebar3 as test shell
    ```
    ```erlang
    1> doctest:module(greeting).
    ```
- Or via `rebar3 eunit`

Both produce the same output:
```shell
 PASS  ./src/greeting.erl:6 -moduledoc
 FAIL  ./src/greeting.erl:19 -doc

    ❌ assertEqual

    Expected: "Hello, World!"
    Received: "Hello, Joe!"

    │
 19 │ 1> greeting:print().
 20 │ "Hello, World!"
    │
    └── at ./src/greeting.erl:19

 PASS  ./src/greeting.erl:29 @doc
 FAIL  ./src/greeting.erl:32 @doc

    ❌ assertEqual

    Expected: true
    Received: false

    │
 32 │ %% 2> Greeting =:= "Hello, World!".
 33 │ %% true
    │
    └── at ./src/greeting.erl:32



Tests: 2 failed, 2 passed, 4 total
 Time: 0.014 seconds
```

> **Note**
>
> The output above is by using the `doctest_eunit_report` as the EUnit report.

## Doctest EUnit Reporter

There is a built-in EUnit reporter called `doctest_eunit_report` to display the tests results correctly. Set it in the EUnit options of the project options, e.g.:

```erlang
% rebar3.config
{eunit_opts, [no_tty, {report, {doctest_eunit_report, []}}]}.
```

An example of the `doctest_eunit_report` output:
![doctest_eunit_report](/assets/reporter-go-to-definition.gif)

## TODO

- [ ] More tests
- [ ] Specs
- [ ] Improve docs

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
