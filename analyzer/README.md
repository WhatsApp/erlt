# Analyzer

*A minimalistic analyzer to get some numbers about properties of your Erlang 
project.*

This is a Scala tool that utilises the OTP `jinterface`
to communicate with a running Erlang node via `RPC.scala`. The Erlang node is used
to load the project's AST, and some analysis can be written in Erlang and done
on that node when it is more convenient.

In the Scala portion of the project, there are two models of Erlang code:
- The lower-level `erlang.Data` which represents the given code purely in terms of
tuples, atoms, lists, etc., and maps most cleanly to the AST that the Erlang node
returns.
- The higher-level `erlang.forms.AbstractForm` and related types, which has
notions of modules, exports, etc., which encodes more semantics
of the code, and therefore makes it feel more natural to inspect.
- `erlang.CErl` - Scala's representation of `cerl:cerl()`. - Experimental WIP
stuff. There are ideas of using this representation in eqwalizer. It's put into
analyzer for now to ease development and experimentation. One nice feature of 
`cerl` - lexical scoping.

## How to configure and run

This analyzer assumes that you build your project with rebar3.

Since this analyzer reads `.beam` files, you'll need to have built your project
before running the tool.

Settings are controlled by three text files: `root`, `paths`, `third_party`
(see [`CodeDirs.scala`](src/main/scala/com/whatsapp/analyzer/CodeDirs.scala)).
Instructions for creating these files are below, in the [configure](#configure) section.

 - `root` holds the path to the root of the Erlang project to analyse
 - `paths` holds paths to the various build output directories of the project
 - `third_party` holds a list of libraries to ignore (they're "third party" in
 the sense that they're irrelevant to this analysis)

### Configure

1. Build your project with `rebar3 compile` (don't miss this step!)
2. Make sure your config files are correct:
   - Put the full absolute path to the root of the rebar project into the `root` file, e.g.:

       ```bash
       echo "/my/project/dir/here/" > root
       ```

   - Run `rebar3 path` - and copy the output AS IS into the `paths` file, e.g.:

       ```bash
       # Compute paths directly from the root file
       root_dir=$(<root)
       pushd "${root_dir}"
       paths_contents=$(rebar3 path)
       popd
       echo "${paths_contents}" > paths
       ```

   - Put a list of SPACE SEPARATED lib names you would like to exclude from analysis into the `third_party` file, e.g.:

       ```bash
       # Leave empty for now
       touch -a third_party
       ```

### Run

Make sure you've configured `root`, `paths` and `third_party` before continuing.

1. Start the analyzer Erlang backend (by building and initializing a named Erlang node with the current working directory on its path):

   ```erlc +debug_info analyzer.erl; erl -sname analyzer@localhost -pa .```

2. Execute a Scala analysis entrypoint in another terminal window:

   ```sbt "runMain {analysisClass}"```
 
   e.g. `sbt "runMain com.whatsapp.analyzer.Behaviours"`

  If the code being analyzed has particularly deep expressions, you may need to
  increase your stack size to accomodate it, e.g.:

  ```export SBT_OPTS="-Xss1000M"```

## Implemented analyses

- `com.whatsapp.analyzer.Behaviours` - stats about implemented behaviours
- `com.whatsapp.analyzer.BifClashes` - reports "bif clashes": when a function
   clashes with an internal BIF (consult `erl_internal:bif/2`)
- `com.whatsapp.analyzer.DynamicCalls` - stats about dynamic calls
   (`M:F(Arg, ...)`, etc)
- `com.whatsapp.analyzer.ErrorHandling` - stats about `catch`-expressions,
   `try`-expressions, etc
- `com.whatsapp.analyzer.GenServerCalls` - report numbers about whether
   the "protocol part" of `gen_server:call(?MODULE, {protocol_part, ...})` 
   is known at compile time or not -  `gen_server:call(?MODULE, {Request, ...})`
- `com.whatsapp.analyzer.GenServerFeatures` - report numbers about which
    `gen_server` features are used, e.g. is `noreply` returned from a call,
    whether `handle_continue` is defined, etc.
- `com.whatsapp.analyzer.HighLevelStats` - number of projects, modules,
   generated files, etc
- `com.whatsapp.analyzer.IntersectionTypes` - find specs with 
   "intersection/dependent" types
- `com.whatsapp.analyzer.ModCycles` - reports cycles between modules 
   (it takes into account only functions for now)
- `com.whatsapp.analyzer.NamedFuns` - reports named 
  [fun expressions](https://erlang.org/doc/reference_manual/expressions.html#fun-expressions). 
  Since such named fun expressions are rare, it reports detailed information (module and line locations).
- `com.whatsapp.analyzer.ParameterizedTypes` - reports parameterized types defined in a project.     
- `com.whatsapp.analyzer.OtpFunsUsage` - stats about which functions from
OTP libs are used in the project.
- `com.whatsapp.analyzer.Primitives concurrency` - stats about using
   concurrency primitives: `send`, `spawn`, etc
- `com.whatsapp.analyzer.Primitives dynamic` - stats about using
   dynamic "primitives": `apply/2`, `apply/3`
- `com.whatsapp.analyzer.RangeTypes` - stats about `X..Y` types   
- `com.whatsapp.analyzer.Receives` - stats about using `receive`s
- `com.whatsapp.analyzer.RedefinedRecordTypes` - see below
- `com.whatsapp.analyzer.UnnamedFuns` - reports the total number of unnamed
  [fun expressions](https://erlang.org/doc/reference_manual/expressions.html#fun-expressions).

## Data interpretation

### Redefined record types

*What?*

```erlang
-record(rec, {id1 :: integer(), id2 :: integer()}).
-type atom_rec() :: #rec{id1 :: atom(), id2 :: atom()}.
```

Erlang doesn't allow to have records with type parameters. However, it allows to override the type of a field.

`RedefinedRecordTypes` analysis lists places in a project where such types appear.

It turns out that in WA code base there are just 3 (three) such places:

```
thriftrpc_client_channel.erl:20 (channel record)
wa_queue.erl:37 (wa_queue record)
wa_voip_ip_cache.erl:129 (addr2 record)
``` 

This simple analysis shows that such a feature is not used, - also comments 
around `thriftrpc_client_channel:channel` show that this feature is
- Confusing
- Not supported properly by dialyzer, - which forces people to introduce even more hacks.

### Range types

Analysis of WA code base shows:
- There are ~30 places with range types
- All of them are used for documentation purposes - there are no operations (addition/subtraction/etc): 
  basically, to check that a given constant falls into a specified range.

Easy to type.

### Named fun expressions

Run `UnnamedFuns` and `NamedFuns` inspections.

In WA codebase (Jan 2021):

- 32143 unnamed funs
- 10 named funs


### Parameterized types

Run `ParameterizedTypes` inspection.

In WA codebase (Jan 2021) there are just 11 (eleven) parameterized types.

Theory to check - all of them when used -> delegate to OTP stuff.
