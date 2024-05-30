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
-module(doctest_term).

% API functions
-export([write/1, format/1]).

% Macros
-define(is_string(X), (is_list(X) orelse is_binary(X))).
-define(is_attrs(X), (is_list(X) orelse is_tuple(X))).

%%%=====================================================================
%%% API functions
%%%=====================================================================

% TODO: Global attrs, e.g.:
%       1> write([{"foo", {fg, blue}}, "bar"], [{bg, red}]).
%       That results in "foo" and "bar" with a red background.
write(Term) ->
    fwrite("~ts~n", [to_bin(format(Term))]).

format(Str) when is_binary(Str) ->
    Str;
format({fmt, Fmt, Args}) ->
    format_bin(Fmt, Args);
format({to_bin, Term}) ->
    to_bin(Term);
format({{fmt, Fmt, Args}, Attrs}) ->
    format({format_bin(Fmt, Args), Attrs});
format({{to_bin, Term}, Attrs}) ->
    format({to_bin(Term), Attrs});
format({Str, Attrs}) when is_binary(Str), is_list(Attrs) ->
    f(Str, parse_attrs(Attrs));
format({Str, Attr}) when is_binary(Str), is_tuple(Attr) ->
    f(Str, parse_attrs([Attr]));
format({Str, Attr}) when is_binary(Str), is_atom(Attr) ->
    f(Str, parse_attrs([Attr]));
format(Attrs) when is_list(Attrs) ->
    [format(Attr) || Attr <- Attrs].

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

f(Str, Attrs) ->
    format_bin("\e[~sm~ts\e[0m", [to_bin(Attrs), Str]).

fwrite(Fmt, Args) ->
    io:fwrite(user, Fmt, Args).

parse_attrs(Attrs) when is_list(Attrs) ->
    to_bin(lists:join($;, [parse_attr(Attr) || Attr <- Attrs])).

% See https://stackoverflow.com/a/33206814/14166584
parse_attr(Attr) when is_integer(Attr), Attr >= 0, Attr =< 107 ->
   integer_to_binary(Attr);
% TODO: Deal with \r\n and \r
parse_attr(nl) ->
    <<"\n">>;
parse_attr({nl, N}) when N >= 1 ->
    to_bin(lists:duplicate(N, <<"\n">>));
parse_attr(ws) ->
    <<"\s">>;
parse_attr({ws, N}) when N >= 1 ->
    to_bin(lists:duplicate(N, <<"\s">>));
parse_attr(tab) ->
    <<"\t">>;
parse_attr({tab, N}) when N >= 1 ->
    to_bin(lists:duplicate(N, <<"\t">>));
parse_attr(reset) ->
    <<"0">>; % Reset / Normal | all attributes off.
parse_attr(bold) ->
    <<"1">>; % Bold or increased intensity | .
parse_attr(faint) ->
    <<"2">>; % Faint (decreased intensity) | Not widely supported.
parse_attr(italic) ->
    <<"3">>; % Italic | Not widely supported. Sometimes treated as inverse.
parse_attr(underline) ->
    <<"4">>; % Underline
parse_attr(slow_blink) ->
    <<"5">>; % Slow Blink | less than 150 per minute
parse_attr(rapid_blink) ->
    <<"6">>; % Rapid Blink | MS-DOS ANSI.SYS; 150+ per minute; not widely supported
parse_attr(reverse_video) ->
    <<"7">>; % [[reverse video]] | swap foreground and background colors
parse_attr(conceal) ->
    <<"8">>; % Conceal | Not widely supported.
parse_attr(crossed_out) ->
    <<"9">>; % Crossed-out | Characters legible, but marked for deletion. Not widely supported.
parse_attr({font, 1}) ->
    <<"10">>; % Primary(default) font
parse_attr({font, 2}) ->
    <<"11">>; % Alternate font.
parse_attr({font, 3}) ->
    <<"12">>; % Alternate font.
parse_attr({font, 4}) ->
    <<"13">>; % Alternate font.
parse_attr({font, 5}) ->
    <<"14">>; % Alternate font.
parse_attr({font, 6}) ->
    <<"15">>; % Alternate font.
parse_attr({font, 7}) ->
    <<"16">>; % Alternate font.
parse_attr({font, 8}) ->
    <<"17">>; % Alternate font.
parse_attr({font, 9}) ->
    <<"18">>; % Alternate font.
parse_attr({font, 10}) ->
    <<"19">>; % Alternate font.
parse_attr(fraktur) ->
    <<"20">>;
% parse_attr(_NotImplementedYet) ->
    % 21; % Bold off or Double Underline | Bold off not widely supported; double underline hardly ever supported.
% parse_attr(_NotImplementedYet) ->
    % 22; % Normal color or intensity | Neither bold nor faint
% parse_attr(_NotImplementedYet) ->
    % 23; % Not italic, not Fraktur |
% parse_attr(_NotImplementedYet) ->
    % 24; % Underline off | Not singly or doubly underlined
% parse_attr(_NotImplementedYet) ->
    % 25; % Blink off |
% parse_attr(_NotImplementedYet) ->
    % 27; % Inverse off |
% parse_attr(_NotImplementedYet) ->
    % 28; % Reveal | conceal off
% parse_attr(_NotImplementedYet) ->
    % 29; % Not crossed out |
parse_attr({fg, Color}) when is_atom(Color) ->
    format_bin("~p", [ansi_fg_color(Color)]);
parse_attr({fg, 30}) ->
    <<"30">>; % Set foreground color
parse_attr({fg, 31}) ->
    <<"31">>; % Set foreground color
parse_attr({fg, 32}) ->
    <<"32">>; % Set foreground color
parse_attr({fg, 33}) ->
    <<"33">>; % Set foreground color
parse_attr({fg, 34}) ->
    <<"34">>; % Set foreground color
parse_attr({fg, 35}) ->
    <<"35">>; % Set foreground color
parse_attr({fg, 36}) ->
    <<"36">>; % Set foreground color
parse_attr({fg, 37}) ->
    <<"37">>; % Set foreground color
parse_attr({fg, {R, G, B}}) ->
    format_bin("38;5;~p", [rgb_to_ansi_color(R, G, B)]);
parse_attr({fg, 39}) ->
    <<"39">>; % Default foreground color | implementation defined (according to standard)
parse_attr({fg, 90}) ->
    <<"90">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 91}) ->
    <<"91">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 92}) ->
    <<"92">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 93}) ->
    <<"93">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 94}) ->
    <<"94">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 95}) ->
    <<"95">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 96}) ->
    <<"96">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg, 97}) ->
    <<"97">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({bg, Color}) when is_atom(Color) ->
    format_bin("~p", [ansi_bg_color(Color)]);
parse_attr({bg, 40}) ->
    <<"40">>; % Set background color
parse_attr({bg, 41}) ->
    <<"41">>; % Set background color
parse_attr({bg, 42}) ->
    <<"42">>; % Set background color
parse_attr({bg, 43}) ->
    <<"43">>; % Set background color
parse_attr({bg, 44}) ->
    <<"44">>; % Set background color
parse_attr({bg, 45}) ->
    <<"45">>; % Set background color
parse_attr({bg, 46}) ->
    <<"46">>; % Set background color
parse_attr({bg, 47}) ->
    <<"47">>; % Set background color
parse_attr({bg, {R, G, B}}) ->
    format_bin("48;5;~p", [rgb_to_ansi_color(R, G, B)]);
parse_attr({bg, 49}) ->
    <<"49">>; % Default background color | implementation defined (according to standard)
parse_attr({bg, 100}) ->
    <<"100">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 101}) ->
    <<"101">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 102}) ->
    <<"102">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 103}) ->
    <<"103">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 104}) ->
    <<"104">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 105}) ->
    <<"105">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 106}) ->
    <<"106">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg, 107}) ->
    <<"107">>; % Set bright background color | aixterm (not in standard.
parse_attr(framed) ->
    <<"51">>; % Framed
parse_attr(encircled) ->
    <<"52">>; % Encircled.
parse_attr(overlined) ->
    <<"53">>; % Overlined.
% parse_attr(_NotImplementedYet) ->
    % 54; % Not framed or encircled.
% parse_attr(_NotImplementedYet) ->
    % 55; % Not overlined.
% parse_attr(_NotImplementedYet) ->
    % 60; % ideogram underline | hardly ever supported.
% parse_attr(_NotImplementedYet) ->
    % 61; % ideogram double underline | hardly ever supported.
% parse_attr(_NotImplementedYet) ->
    % 62; % ideogram overline | hardly ever supported.
% parse_attr(_NotImplementedYet) ->
    % 63; % ideogram double overline | hardly ever supported.
% parse_attr(_NotImplementedYet) ->
    % 64; % ideogram stress marking | hardly ever supported.
% parse_attr(_NotImplementedYet) ->
    % 65; % ideogram attributes off | reset the effects of all of 60-64.
parse_attr({fg_bright, 1}) ->
    <<"90">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 2}) ->
    <<"91">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 3}) ->
    <<"92">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 4}) ->
    <<"93">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 5}) ->
    <<"94">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 6}) ->
    <<"95">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 7}) ->
    <<"96">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({fg_bright, 8}) ->
    <<"97">>; % Set bright foreground color | aixterm (not in standard).
parse_attr({bg_bright, 1}) ->
    <<"100">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 2}) ->
    <<"101">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 3}) ->
    <<"102">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 4}) ->
    <<"103">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 5}) ->
    <<"104">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 6}) ->
    <<"105">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 7}) ->
    <<"106">>; % Set bright background color | aixterm (not in standard.
parse_attr({bg_bright, 8}) ->
    <<"107">>; % Set bright background color | aixterm (not in standard.
parse_attr(clear_line) ->
    <<"2K">>; % Clear Line,
parse_attr(cursor_pos) ->
    <<"6n">>;
parse_attr({cursor_pos, {Ln, Col}}) ->
    format_bin("~p;~pH", [Ln, Col]); % Put the cursor at line L and column C.
parse_attr({cursor_up, N}) ->
    format_bin("~pA", [N]); % Move the cursor up N lines")
parse_attr(cursor_up) ->
    <<"1A">>; % Move the cursor up 1 line
parse_attr({cursor_down, N}) ->
    format_bin("~pB", [N]); % Move the cursor down N lines")
parse_attr(cursor_down) ->
    <<"1B">>; % Move the cursor down 1 line
parse_attr({cursor_right, N}) ->
    format_bin("~pC", [N]); % Move the cursor forward N columns")
parse_attr(cursor_right) ->
    <<"1C">>; % Move the cursor forward 1 line
parse_attr({cursor_left, N}) ->
    format_bin("~pD", [N]); % Move the cursor backward N columns\n")
parse_attr(cursor_left) ->
    <<"1D">>; % Move the cursor backward 1 line
parse_attr(clear) ->
    <<"2J">>; % Clear the screen, move to (0,0)")
parse_attr(erase) ->
    <<"K">>; % Erase to end of line
parse_attr(cursor_save) ->
    <<"s">>; % Save cursor position
parse_attr(cursor_restore) ->
    <<"u">>. % Restore cursor position

ansi_fg_color(Color) ->
    case Color of
        black -> 30;
        red -> 31;
        green -> 32;
        yellow -> 33;
        blue -> 34;
        magenta -> 35;
        cyan -> 36;
        white -> 37;
        bright_black -> 90;
        bright_red -> 91;
        bright_green -> 92;
        bright_yellow -> 93;
        bright_blue -> 94;
        bright_magenta -> 95;
        bright_cyan -> 96;
        bright_white -> 97
    end.

ansi_bg_color(Color) ->
    ansi_fg_color(Color) + 10.

rgb_to_ansi_color(R, R, R) when R < 8 ->
    16;
rgb_to_ansi_color(R, R, R) when R > 248 ->
    231;
rgb_to_ansi_color(R, R, R) ->
    round(((R - 8) / 247) * 24 + 232);
rgb_to_ansi_color(R, G, B) ->
    16
    + (36 * round(R / 255 * 5))
    + (6 * round(G / 255 * 5))
    + round(B / 255 * 5).

format_bin(Fmt, Args) ->
    to_bin(io_lib:format(Fmt, Args)).

to_bin(Bin) when is_binary(Bin) ->
    Bin;
to_bin(Str) when is_list(Str) ->
    unicode:characters_to_binary(Str);
to_bin(Int) when is_integer(Int) ->
    integer_to_binary(Int);
to_bin(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_bin(Term) ->
    unicode:characters_to_binary(io_lib:format("~p", [Term])).
