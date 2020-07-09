# Analyzer

A minimalistic analyzer to get some numbers about properties of your Erlang 
project.

## How to run

1. Configure files accordingly: `paths`, `root`, `third_party`
2. Setup analyzer Erlang backend: 
   `erlc +debug_info analyzer.erl; erl -sname analyzer@localhost -pa .`
3. Run an analysis from sbt (`runMain {analysisClass}`)

## Implemented analyses

- `com.whatsapp.analyzer.Behaviours` - stats about implemented behaviours
- `com.whatsapp.analyzer.BifClashes` - reports "bif clashes": when a function
   clashes with an internal BIF (consult `erl_internal:bif/2`)
- `com.whatsapp.analyzer.DynamicCalls` - stats about dynamic calls
   (`M:F(Arg, ...)`, etc)
- `com.whatsapp.analyzer.ErrorHandling` - stats about `catch`-expressions,
   `try`-expressions, etc
- `com.whatsapp.analyzer.HighLevelStats` - number of projects, modules,
   generated files, etc
- `com.whatsapp.analyzer.OtpFunsUsage` - stats about which functions from
OTP libs are used in the project.
- `com.whatsapp.analyzer.Primitives concurrency` - stats about using
   concurrency primitives: `send`, `spawn`, etc
- `com.whatsapp.analyzer.Primitives dynamic` - stats about using
   dynamic "primitives": `apply/2`, `apply/3`
- `com.whatsapp.analyzer.Receives` - stats about using `receive`s
