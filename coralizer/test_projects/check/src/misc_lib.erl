-module(misc_lib).

-compile([export_all, nowarn_export_all]).

-spec boolean_id(boolean()) -> boolean().
boolean_id(B) -> B.
