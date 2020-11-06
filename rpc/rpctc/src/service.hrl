-record(service, {
    name = undefined :: string(),
    singleton = false :: boolean(),
    calls = [] :: list(call),
    casts = [] :: list(cast)
}).

-record(call, {
    name = "" :: string(),
    params = [] :: list(string()),
    return = [] :: list(string())
}).

-record(cast, {
    name = "" :: string(),
    params = [] :: list(string())
}).
