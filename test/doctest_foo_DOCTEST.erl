%%%---------------------------------------------------------------------
%%% Tests generated via doctest.
%%% - module: doctest_foo
%%% - file: /home/williamthome/Projects/williamthome/doctest/test/support/doctest_foo.erl
%%%---------------------------------------------------------------------
-module(doctest_foo_DOCTEST).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sum_2_test() ->
    [
        ?assertEqual(doctest_foo:sum(1, 1), 2),
        ?assertEqual(doctest_foo:sum(1, 2), 3)
    ].

mult_2_test() ->
    [
        ?assertEqual(doctest_foo:mult(1, 1), 1),
        ?assertEqual(doctest_foo:mult(1, 2), 2)
    ].

concat_2_test() ->
    [
        ?assertEqual(doctest_foo:concat( "foo", "bar" ), "foobar")
    ].

-endif.
