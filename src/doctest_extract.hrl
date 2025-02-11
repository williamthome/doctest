-define(CODE_BLOCK_RE,
    "(?ms)^(```[`]*)(?:erlang)?\\s*\\n" % ```erlang
    "(.*?)"                             % <erlang-code>
    "(?:\\n^(\\1)(\\s+|\\n|$))"         % ```
).

