# doctest

A library to test Erlang `-moduledoc` and `-doc` attributes.

## Requirements

OTP >= 27.

## Installation

```erlang
% rebar.config
{profiles, [
    {test, [
        {deps, [{doctest, "0.7.0"}]}
    ]}
]}.
```

## Usage

Tests run via the `doctest:module/1,2` function or on modules that include the [doctest header](/include/doctest.hrl), but only exported functions are tested.

### Testing via doctest:module/1,2 function

Take this module:
````erlang
-module(foo).
-moduledoc """
Module doc tags can also be tested.

```erlang
1> foo:foo() =:= bar.
true
```
""".

-export([foo/0]).

-doc """
```erlang
1> foo:foo().
foo
```
""".
foo() ->
    bar.
````

Running it via `rebar3 as test shell`:
```erlang
1> doctest:module(foo).
.F
Failures:

  1) foo:foo/0: -doc | Ln 13
     Failure/Error: ?assertEqual(foo, foo:foo())
       expected: foo
            got: bar
     %% eunit_proc.erl:583:in `eunit_proc:run_group/2`
     Output:
     Output:

Finished in 0.010 seconds
2 tests, 1 failures
```

#### Options

Options can be provided when using the `doctest:module/2` function. The available options are:
- `moduledoc` :: `boolean()`: enable or disable `-moduledoc` test
- `funs` :: `boolean()` | `[{atom(), arity()}]`: enable or disable `-doc` tests or define the functions to be tested
- `eunit` :: `resolve | term()`: set [EUnit options](#eunit-options)

### Testing via doctest header

Take this module:
````erlang
-module(math).

-export([add/2]).

-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
% -doctest <see the options section>.
-endif.

-doc """
Adds two numbers together.

_Example_:
```erlang
1> math:add(0, 1).
1
2> math:add(
.. 1,
.. 1
.. ).
2
```
""".
add(A, B) ->
    A+B.
````

> Note that the code is defined like in the Erlang shell, starting the expression line from `1..N` and the `^([1-9][0-9]*)>\s` format, and continues in multiple lines with `..` and the format `^(\s*)\.\.\s`. The expression header and multiple lines must be aligned, for example:
> ```erlang
> % Valid
> 1> foo.
> foo
> 2> foo
> .. =:=
> .. bar.
> bar
>
> % Invalid
> 1> foo.
> foo
> 200> foo % <- Must 2 (previous line + 1)
>  .. =:=  % <- Must be aligned
>   .. bar.
> bar
> ```

Now, by running `rebar3 eunit`:
```shell
Finished in 0.018 seconds
2 tests, 0 failures
```

By changing the first test to:
```erlang
1> math:add(1, 1).
1
```

And running `rebar3 eunit` again:
```shell
Failures:

  1) math:add/2: -doc | Ln 15
     Failure/Error: ?assertEqual(1, math:add(1, 1))
       expected: 1
            got: 2
     %% eunit_proc.erl:583:in `eunit_proc:run_group/2`
     Output:
     Output:

Finished in 0.010 seconds
2 tests, 1 failures
```

#### Options

Options are defined via the `-doctest` attribute and can be defined multiple times. The available options are:
- `boolean()` | `{enabled, boolean()}`: enable or disable the test running, e.g.:
  ```erlang
  -doctest true.
  ```
- `{moduledoc, boolean()}`: enable or disable `-moduledoc` test, e.g.:
  ```erlang
  -doctest {moduledoc, true}.
  ```
- `[{atom(), arity()}]` | `{funs, [{atom(), arity()}] | boolean()}`: enable or disable `-doc` tests or define the functions to be tested, e.g.:
  ```erlang
  -doctest [add/2].
  ```
- `eunit` :: `resolve | term()`: set [EUnit options](#eunit-options), e.g.:
  ```erlang
  -doctest {eunit, resolve}.
  ```
- `map()`: define all or partial options, e.g.:
  ```erlang
  -doctest #{
      enabled => true,
      moduledoc => true,
      funs => [add/2],
      eunit => resolve
  }.
  ```

### Global options

Options can be globally defined via a [config file](https://www.erlang.org/doc/man/config.html), e.g.:
```erlang
% config/sys.config
[{doctest, [
    {enabled, true},
    {moduledoc, false},
    {funs, true},
    {eunit, [no_tty, {report, {eunit_progress, [colored, profile]}}]}
]}].
```

### EUnit options

Valid EUnit options are the `resolve` atom and a proplist.

By defining `resolve` as the EUnit options, `doctest` will try to resolve the options via `rebar`. Custom options can be defined as documented at [rebar3.org](https://rebar3.org/docs/testing/eunit/#eunit_opts):

> The default EUnit options can be configured, as documented [here](https://www.erlang.org/doc/man/eunit.html#test-2).
>
> Interesting undocumented options are:
>
> - `no_tty` completely disables the default EUnit reporter output
> - `{report, {Module, Args}}` runs a custom EUnit reporter (the functionality that prints results to the shell). The reporter module needs the following callbacks implemented:
>   ```erlang
>   -export([start/0]).
>   -export([start/1]).
>   -export([init/1]).
>   -export([handle_begin/3]).
>   -export([handle_end/3]).
>   -export([handle_cancel/3]).
>   -export([terminate/2]).
>   ```
>
> `no_tty` and `report` can be combined to replace the EUnit reporter with a custom one:
> ```erlang
> {eunit_opts, [no_tty, {report, {my_reporter, Opts}}]}.
> ```

#### Doctest EUnit Reporter

`doctest` has a built-in EUnit reporter called `doctest_eunit_report` for better visualization of the tests. It can be used by defining the below option in the `rebar.config` of your project:
```erlang
{eunit_opts, [no_tty, {report, {doctest_eunit_report, []}}]}.
```

> Currently, no options are expected/provided by the reporter.

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
