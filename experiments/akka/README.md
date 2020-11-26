# Akka Example

This example demonstrates what generic server-style code looks like in Akka.

The reason for looking to Akka is that is a mature ecosystem for typed Erlangish concurrency.

Currently this README only discusses only some of the many ideas from the code in this directory–please see comments in the code for more details about what these demonstrate:
- `GenServerMatchTypes.scala` shows how to use match types to map request types to response types. More on this below.
- `BasicBlocks.scala` shows how things comparable to `gen_server:call` and `gen_server_cast` are typed in Akka.

## IDE

- sbt
- VSCode
- Then follow [these instructions], under "Usage". TLDR: `sbt launchIDE` should work if `code` is on your PATH.

## Run the Examples

```sh
> sbt # to enter the sbt shell
> run # then select which example to run
```

You can choose between the examples:

[!sbt dialog for choosing an example](https://user-images.githubusercontent.com/273180/99680533-0e09de00-2a75-11eb-873c-54d5b9eb1d4e.png)

## On Match Types

> Prefer reading the code to this README

When typing a GenServer, what we'd like to express is *which* replies are OK for which requests.

Some pseudo-erlt for this:

```erl
-enum call() :: (
              equal{n : integer()}
             , closer{n1: integer(), n2: integer()}
).

-enum reply() :: (
              equal{n : integer()}
             , closer{n1: integer(), n2: integer()}
).


% The following is made-up Erlangy syntax corresponding to a scala match type
-protocol call_reply ::
    call.equal -> reply.equal
    call.closer -> reply.closer.

-implement_protocol(call_reply).
handle_call(call.equal{X}, State) -> reply.equal{State =:= X};
handle_call(call.closer{X, Y}, State) ->
    Closer = ...code_here...,
    reply.closer{Closer};
```

This is what Scala match types really look like:

```scala
    // define the procol
    type CallReplyProtocol[C] = C match
      case Call.Equal  => EqualReply
      case Call.Closer => CloserReply

    // implement the procol
    def reply[C <: Counter.Call](c: C, s: Counter.State): CallReplyProtocol[C] = c match
      case x: Counter.Call.Equal  =>
        Counter.EqualReply(x.n == s)
      case x: Counter.Call.Closer =>
        val closer = if (Math.abs(x.n1 - s) < Math.abs(x.n2 - s)) then x.n1 else x.n2
        Counter.CloserReply(closer)

```

> If you have questions about the semantics, the best way to get an answer is to run the code in this directory.

The docs for match types are in https://dotty.epfl.ch/docs/reference/new-types/match-types.html

- match types separate the data from how the data is used: the calculation of which reply type to use for a given call type is in the hands of the *user* of the enum rather than the definer of the enum. That's why they require more typing (on the keyboard) than GADTs for small examples.

> Note: It may be useful to compare match types in `./GenServerMatchTypes.scala` against the GADTs in `../experiments/Haskell\ simulation`.

- match types allow matching against constructors of multiple enums in a single pattern expression. We may or may not want this capability for ErlT.

- match types in Scala have [several restrictions](The docs for match types are in https://dotty.epfl.ch/docs/reference/new-types/match-types.html): for each of these we may want to see whether the same restrictions would apply for Erlt.

