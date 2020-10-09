-module(rebar_prv_erlt_clean).

%
% rebar3 plugin docs: https://www.rebar3.org/docs/tutorials/building_plugins/
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(NAMESPACE, erlt).
-define(DEPS, [{default, app_discovery}, {default, clean}]).

-define(EXAMPLE,
    "clean all apps in the project: `rebar3 erlt clean`~n\tclean a single app: `rebar3 erlt <app-name>`"
).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, ?EXAMPLE},
            {opts, []},
            {short_desc, "clean"},
            {
                desc,
                "clean~n"
                ?EXAMPLE
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok | error, rebar_state:t()}.
do(State) ->
    rebar_prv_erlt:run(fun rebar_prv_erlt:clean_app/1, "Cleaning ErlT build artifacts", State).

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
