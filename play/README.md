# Play

## Prerequisites

- Download the latest sterlang (type checker) for your platform from https://github.com/whatsapp/erlt/releases.
- Rename it to `sterlang` and place it in `<this repo root>/erltc/priv`.
- Then cd `<this repo root>/play && rebar3 new erlt <app-name>`

## Make a new app

```sh
rebar3 erlt new <app-name>
```

Then `cd  <app-name>` and:
- edit the ErlT source code
- `rebar3 compile` to compile, `rebar3 shell` to interact with your app, `rebar3 escriptize` etc.

**Have fun!**

> PRs to this directory with your example ErlT code are welcome, just please take care not to give away company secrets.
