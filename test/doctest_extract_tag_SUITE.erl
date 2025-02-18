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
-module(doctest_extract_tag_SUITE).
-compile([export_all, nowarn_export_all]).

-record(foo, {foo}).
-record(foobar, {foo, bar}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{
        extractors => [doctest_extract_tag],
        bindings => #{'M' => ?MODULE},
        records => [
            {foo, record_info(fields, foo)},
            {foobar, record_info(fields, foobar)}
        ]
    }).
-endif.

% @doc
% ```
% > M:single_line_test().
% ok
% '''
single_line_test() ->
    ok.

% @doc
% ```
% > M:multi_line_test().
% [
%     a,
%     b,
%     b
% ]
% > M:multi_line_test().
% [
%     a,
%     b,
%     b
% ]
% '''
% 
% ```
% > M:multi_line_test()
%   =/= [].
% true
% '''
multi_line_test() ->
    [
        a,
        b,
        b
    ].

% @doc
% ```
% > A = 1.
% > B = 1.
% > A + B.
% 2
% '''
% 
% ```
% > A 
%   = 1.
% > B 
%   = 1.
% > A + B.
% 2
% '''
skip_line_test() ->
    ok.

% @doc
% ```
% > % before expr
%   M:comment_test(). % after expr
% ok
% '''
comment_test() ->
    ok.

% @doc
% ```
% 1> % before expr
% .. M:shell_format_test(). % after expr
% ok
% 2> A = 1.
% 3> B = 1.
% 4> A + B.
% 2
% '''
shell_format_test() ->
    ok.

% @doc
% ```
% > Foo0 = #foo{}.
% > Foo = Foo0#foo{foo = foo}.
% > Foobar = #foobar{foo = foo, bar = bar}.
% #foobar{foo = foo, bar = bar}
% > M:records_test().
% [#foo{foo = foo}, #foobar{foo = foo, bar = bar}]
% '''
records_test() ->
    [#foo{foo = foo}, #foobar{foo = foo, bar = bar}].
