# Play

## Prerequisites

- Download the latest sterlang (type checker) for your platform from https://github.com/whatsapp/erlt/releases.
- Rename it to `sterlang` and place it in `<this repo root>/erltc/priv`.

## Make a new app

In the the same directory as this README:

```sh
../scripts/rebar3 new erlt <app-name>
```

Alternatively, with rebar3 version 3.14.2 or newer

```sh
rebar3 new erlt <app-name> # with rebar3 version 3.14.2 or newer
```

Then `cd  <app-name>` and:
- edit the ErlT source code
- `../../scripts/rebar3 compile` to compile, `../../scripts/rebar3 shell` to interact with your app, `../../scripts/rebar3 escriptize` etc.

> Alternatively, use rebar3 version 3.14.2 or newer instead of ../../scripts/rebar3

**Have fun!**

> PRs to this directory with your example ErlT code are welcome, just please take care not to give away company secrets.
