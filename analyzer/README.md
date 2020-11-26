# Analyzer

*A minimalistic analyzer to get some numbers about properties of your Erlang 
project.*

This is a Scala tool that it utilises the OTP `jinterface`
to communicated with running Erlang code via `RPC.scala`. Erlang is used
to load the project's AST, and some analysis can be done there
when that is more convenient.

Since this analyzer reads `.beam` files, you'll need to have built your project
before running the tool.

## How to configure and run

This analyzer assumes that you build your project with rebar3.

Settings are controlled by three text files: `root`, `paths`, `third_party`
(see [`CodeDirs.scala`](src/main/scala/com/whatsapp/analyzer/CodeDirs.scala)).
Instructions for creating these files are below, in the [configure](###configure) section.

 - `root` holds the path to the root of the Erlang project to analyse
 - `paths` holds paths to the various build output directories of the project
 - `third_party` holds a list of libraries to ignore (they're "third party" in
 the sense that they're irrelevant to this analysis)

### Configure

1. Build your project with `rebar3 compile` (don't miss the step!)
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

1. Start the analyzer Erlang backend (by building and initializing a named Erlang node with the current working directory on its path):

   ```erlc +debug_info analyzer.erl; erl -sname analyzer@localhost -pa .```

2. Execute a Scala analysis entrypoint in another terminal window:

   ```sbt "runMain {analysisClass}"```
   
   e.g. `sbt "runMain com.whatsapp.analyzer.Behaviours"`

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
- `com.whatsapp.analyzer.HighLevelStats` - number of projects, modules,
   generated files, etc
- `com.whatsapp.analyzer.IntersectionTypes` - find specs with 
   "intersection/dependent" types
- `com.whatsapp.analyzer.ModCycles` - reports cycles between modules 
   (it takes into account only functions for now)
- `com.whatsapp.analyzer.OtpFunsUsage` - stats about which functions from
OTP libs are used in the project.
- `com.whatsapp.analyzer.Primitives concurrency` - stats about using
   concurrency primitives: `send`, `spawn`, etc
- `com.whatsapp.analyzer.Primitives dynamic` - stats about using
   dynamic "primitives": `apply/2`, `apply/3`
- `com.whatsapp.analyzer.Receives` - stats about using `receive`s
