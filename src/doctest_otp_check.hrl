% -ifdef(OTP_RELEASE).
%     -if(?OTP_RELEASE < 27).
%         -error("doctest requires OTP 27 or higher.").
%     -endif.
% -else.
%     -error("doctest requires OTP 27 or higher.").
% -endif.

-ifdef(OTP_RELEASE).
    -if(?OTP_RELEASE < 27).
        -define(DOC_ATTRS_SUPPORTED, false).
    -else.
        -define(DOC_ATTRS_SUPPORTED, true).
    -endif.
-else.
    -define(DOC_ATTRS_SUPPORTED, false).
-endif.
