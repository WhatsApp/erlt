-module(rebar_prv_erlt_compile).

-export([init/1, do/1, format_error/1]).

-include("erlt_build_types.hrl").

% rebar3 plugin docs: https://www.rebar3.org/docs/tutorials/building_plugins/
-behaviour(provider).

-define(PROVIDER, compile).
-define(NAMESPACE, erlt).
-define(DEPS, [{default, app_discovery}, {default, compile}]).
-define(SRC_DIR, "src").

-define(EXAMPLE,
    "compile all apps in the project: `rebar3 erlt compile`~n\tbuild a single app: `rebar3 compile erlt <app-name>`"
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
            {short_desc, "compile ErlT files"},
            {
                desc,
                "compile ErlT files~n~n"
                ?EXAMPLE
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    ok = rebar_prv_erlt:run(fun rebar_prv_erlt:compile_app/1, State),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
