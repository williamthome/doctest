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
-module(doctest_extract).
-moduledoc false.

% API functions
-export([extract_module_tests/4, code_blocks/2]).

% Support functions
-export([default_extractor/0, code_block_asserts/2, keep_fun/2]).

% -callback extract_module_tests(Mod, ShouldTestModDoc, FunsOpts) -> Result when
%           Mod :: module(),
%           ShouldTestModDoc :: boolean(),
%           FunsOpts :: boolean() | [{FunName, FunArity}],
%           FunName :: atom(),
%           FunArity :: arity(),
%           Result :: {ok, Tests} | {error, term()},
%           Tests :: list().

-callback code_blocks(binary()) -> code_blocks().

-type code_blocks() :: [{binary(), location()}] | none.
-type location() :: {Ln :: pos_integer(), Col :: pos_integer()}.

% Check OTP version.
-include("doctest_otp_check.hrl").

%%%=====================================================================
%%% API functions
%%%=====================================================================

extract_module_tests(Extractor, Mod, ShouldTestModDoc, FunsOpts) ->
    case Extractor:extract_module_tests(Mod, ShouldTestModDoc, FunsOpts) of
        {ok, Tests} ->
            Desc = iolist_to_binary(io_lib:format("module '~w'", [Mod])),
            {ok, {Desc, Tests}};
        {error, Reason} ->
            {error, Reason}
    end.

code_blocks(Doc, RE) when is_binary(Doc) ->
    case re:run(Doc, RE, [global, {capture, all_but_first, index}]) of
        {match, Groups} ->
            {ok, [{binary_part(Doc, Pos, Len), loc(Doc, Pos)}
                 || [_, {Pos, Len}, _, _] <- Groups]};
        nomatch ->
            none
    end.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

-if(?DOC_ATTRS_SUPPORTED).
default_extractor() ->
    doctest_markdown.
-else.
default_extractor() ->
    doctest_comment.
-endif.

code_block_asserts(CodeBlock, InitLn) ->
    case chunks(split_lines(CodeBlock)) of
        [] ->
            [];
        [H|_] = Chunks ->
            asserts(Chunks, {H, 1}, {InitLn, InitLn}, [])
    end.

keep_fun(Fun, Opts) ->
    case Opts of
        KeepAll when is_boolean(KeepAll) ->
            KeepAll;
        Funs when is_list(Funs) ->
            lists:member(Fun, Funs)
    end.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

loc(Doc, Pos) ->
    Pre = binary_part(Doc, 0, Pos),
    parse_loc(Pre, {1, 1}).

parse_loc(<<$\r, $\n, Rest/binary>>, {Ln, _Col}) ->
    parse_loc(Rest, {Ln+1, 1});
parse_loc(<<$\r, Rest/binary>>, {Ln, _Col}) ->
    parse_loc(Rest, {Ln+1, 1});
parse_loc(<<$\n, Rest/binary>>, {Ln, _Col}) ->
    parse_loc(Rest, {Ln+1, 1});
parse_loc(<<_, Rest/binary>>, {Ln, Col}) ->
    parse_loc(Rest, {Ln, Col+1});
parse_loc(<<>>, {Ln, Col}) ->
    {Ln, Col}.

split_lines(CodeBlock) ->
    binary:split(CodeBlock, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]).

% TODO: Allow comments
chunks(Parts) ->
    Opts = [{capture, all_but_first, binary}],
    lists:map(fun(Part) ->
        case re:run(Part, <<"^([1-9][0-9]*)>\\s(.*?)\\.*$">>, Opts) of
            {match, [N, Left]} ->
                {left, {N, Left}};
            nomatch ->
                case re:run(Part, <<"^(\\s*)\\.\\.\\s(.*?)\\.*$">>, Opts) of
                    {match, [Ws, More]} ->
                        {more, {Ws, More}};
                    nomatch ->
                        {right, Part}
                end
        end
    end, Parts).

asserts([{left, {N, L}}, {more, {Ws, M}} | T], HI, {Ln, NLn}, Acc) ->
    case check_more_format(Ws, N) of
        ok ->
            asserts([{left, {N, <<L/binary, M/binary>>}} | T], HI, {Ln, NLn+1}, Acc);
        {error, {EWs, RWs}} ->
            Expected = iolist_to_binary([lists:duplicate(EWs, "\s"), "..> ", M]),
            Received = iolist_to_binary([lists:duplicate(RWs, "\s"), "..> ", M]),
            {error, {format, #{
                line => NLn+1,
                expected => Expected,
                received => Received
            }}}
    end;
asserts([{left, {N, L}}, {right, R} | T], {{left, {_, H}}, I}, {Ln, NLn}, Acc) ->
    case check_left_index(N, I) of
        ok when T =:= [] ->
            {ok, [{{L, Ln}, {R, NLn+1}} | Acc]};
        ok ->
            asserts(T, {hd(T), I+1}, {NLn+2, NLn+2}, [{{L, Ln}, {R, NLn+1}} | Acc]);
        error ->
            Expected = iolist_to_binary([integer_to_binary(I), "> ", H]),
            Received = iolist_to_binary([N, "> ", H]),
            {error, {format, #{
                line => Ln,
                expected => Expected,
                received => Received
            }}}
    end;
% Code block is not a test, e.g:
% foo() ->
%     bar.
asserts(_, _, _, _) ->
    {ok, []}.

check_more_format(Ws, Ln) ->
    LnSz = max(0, byte_size(Ln) - 1),
    WsSz = byte_size(Ws),
    case WsSz =:= LnSz of
        true ->
            ok;
        false ->
            {error, {LnSz, WsSz}}
    end.

check_left_index(N, Ln) ->
    case catch binary_to_integer(N) =:= Ln of
        true ->
            ok;
        _ ->
            error
    end.
