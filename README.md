# doctest

A library to test Erlang `-moduledoc` and `-doc` attributes.

## Requirements

OTP >= 27.

## Installation

```erlang
% rebar.config
{profiles, [
    {test, [
        {deps, [{doctest, "0.4.0"}]}
    ]}
]}.
```

## Usage

Tests run via the `doctest:module/1,2` function or on modules that include the [doctest header](/include/doctest.hrl).

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

Running it via shell:
```erlang
1> doctest:module(foo).
.F
Failures:

  1) doctest:-parse/4-fun-0-/0:13
     Failure/Error: ?assertEqual(foo, foo:foo())
       expected: foo
            got: bar
     %% eunit_proc.erl:583:in `eunit_proc:run_group/2`
     Output:
     Output:

Finished in 0.013 seconds
2 tests, 1 failures
```

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

> Note that the code is defined like the Erlang shell, starting with `N> `, where `N` is a number, and continues in multiple lines with `.. `. The result is a value without starting with those shell symbols.

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

  1) doctest_parse_transform:-parse/4-fun-0-/0:26
     Failure/Error: ?assertEqual(1, math:add(1, 1))
       expected: 1
            got: 2
     %% eunit_proc.erl:583:in `eunit_proc:run_group/2`
     Output:
     Output:

Finished in 0.010 seconds
2 tests, 1 failures
```

### Options

Options are defined via the `-doctest` attribute and can be defined multiple times.

#### Available options

- `boolean()` | `{enabled, boolean()}`: enable or disable the test running.
  ```erlang
  -doctest true.
  ```
- `{moduledoc, boolean()}`: enable or disable `-moduledoc` test.
  ```erlang
  -doctest {moduledoc, true}.
  ```
- `[{atom(), arity()}]` | `{funs, [{atom(), arity()}] | boolean()}`: define functions to be tested.
  ```erlang
  -doctest [add/2].
  ```
- `map()`: define all or partial options.
  ```erlang
  -doctest #{
      enabled => true,
      moduledoc => true,
      funs => [add/2]
  }.
  ```

### Important

Currently, only exported functions can be tested.

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
