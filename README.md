# doctest

An OTP library to test `-doc` attributes.

## Requirements

OTP >= 27.

## Installation

```erlang
% rebar.config
{profiles, [
    {test, [
        {deps, [{doctest, "0.1.0"}]}
    ]}
]}.
```

## Usage

### Via parse_transform

Take this module:

````erlang
-module(math).
-moduledoc """
A module for basic arithmetic.
""".

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

This will produce the file `<project_cwd>/test/math_DOCTEST.erl` with
the following tests:
```erlang
% <name>_<arity>_test
add_2_test() ->
    [
        ?assertEqual(math:add(0, 1), 1),
        ?assertEqual(math:add( 1, 1 ), 2)
    ].
```

Note that the code is defined like the Erlang shell, starting with `N> `, where `N` is a number,
and continues in multiple lines with `.. `. The result is a value without
starting with those shell symbols.

### Options

Options are defined via the `-doctest` attribute and can be defined multiple times.

#### Available options

- `boolean()`: enable or disable tests.
  ```erlang
  -doctest true.
  ```
- `atom()`: define the test module name.
  ```erlang
  -doctest math_DOCTEST.
  ```
- `proplists:proplist()` | `all`: define the functions to be tested.
  ```erlang
  -doctest [add/2].
  ```
- `string()` | `{abs, string()}` | `{cwd, string()}`: define the test file location.
  ```erlang
  -doctest {abs, "/tmp/doctests"}.
  ```
- `map()`: define all or partial options.
  ```erlang
  -doctest #{
      enabled => true,
      module => math_DOCTEST,
      funs => [add/2],
      location => {abs, "/tmp/doctests"}
  }.
  ```

### Important

Currently, only exported functions can be tested.

## TODO

- [ ] All kinds of tests;
- [ ] Ability to test modules via function and not only via `parse_transform`;
- [ ] Maybe add a mechanism to test non-exported functions, but probably this
makes no sense;
- [ ] Test and fix issues on umbrella applications;
- [ ] Improve docs;
- [ ] Implement `-moduledoc` tests in the same way that for `-doc`,
      but creating a test function called` moduledoc_test/0`.
- [ ] Add support for unbound variables, e.g.:
      ```erlang
      1> Foo = foo.
      foo;
      2> foo =:= Foo.
      true
      ```

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
