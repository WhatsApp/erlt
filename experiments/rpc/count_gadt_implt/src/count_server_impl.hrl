%% If we have type classes, the user wouldn't need to type this.
-spec impl() -> count_server:impl_module(input(), state()).

impl() ->
    #count_server:impl_module{
        init = fun init/1,
        equal = fun equal/2,
        closer = fun closer/3,
        inc = fun inc/2,
        dec = fun dec/2
    }.
