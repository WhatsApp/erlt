-define(IS_ATOMIC(Kind),
    Kind =:= integer orelse
        Kind =:= float orelse
        Kind =:= char orelse
        Kind =:= atom orelse
        Kind =:= string orelse
        Kind =:= var
).

-define(IS_TYPE(Kind),
    Kind =:= type orelse
        Kind =:= opaque orelse
        Kind =:= enum orelse
        Kind =:= struct orelse
        Kind =:= exception orelse
        Kind =:= message orelse
        Kind =:= unchecked_opaque
).

-define(IS_SPEC(Kind),
    Kind =:= spec orelse
        Kind =:= callback
).

-define(IS_FUNCTION(Kind),
    Kind =:= function orelse
        Kind =:= unchecked_function
).
