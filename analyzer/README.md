# Analyzer

A minimalistic analyzer to get some numbers about properties of your Erlang 
project.

## How to run

1. Configure files accordingly: `paths`, `root`, `third_party`
2. Setup analyzer Erlang backend: 
   `erlc +debug_info analyzer.erl; erl -sname analyzer@localhost -pa .`
3. Run an analysis from sbt (`runMain {analysisClass}`)

## How to configure:

This analyzer assumes that you build your project with rebar3.
Settings are handled by [`CodeDirs.scala`](src/main/scala/com/whatsapp/analyzer/CodeDirs.scala) 

1. Put the full absolute path to the root of the rebar project into `root` file
2. Build your project with `rebar3 compile` (don't miss the step!)
3. run `rebar3 path` - and copy the output AS IS into `paths`
4. Put a list of SPACE SEPARATED lib names you would like to exclude from analysis
   into `third_party`.

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
