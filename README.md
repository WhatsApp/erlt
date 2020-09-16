# erl2: Modern Erlang Project

This repository is for prototyping and collaborating around the Modern Erlang project.

## Examples

See the [examples](examples/) folder.

## Requirements

- [erlang](https://www.erlang.org/)
- [sbt](https://www.scala-sbt.org/)

## Building

`erlbuild` (to be removed)

    cd erlbuild; rebar3 escriptize

`erltc` (WIP)

    cd erltc; rebar3 escriptize

`stErlang`:

    cd sterlang; sbt assembly

## Testing

You have to build `erlbuild` and `erltc` first

    make -C examples test


## Development

See [here](doc/01_intro.md#development).

## Full documentation

See the [doc](doc) folder.

## Join the erl2 community

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License
erl2 is Apache licensed, as found in the LICENSE file.
