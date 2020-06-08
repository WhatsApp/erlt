# Analyzer

A minimalistic analyzer to get some numbers about properties of your Erlang 
project.

## How to run

1. Modify `com.whatsapp.analyzer.CodeDirs` to reflect the setting of your 
   project.
2. Setup analyzer Erlang backend: 
   `erlc analyzer.erl; erl -sname analyzer@localhost -pa .`
3. Run an analysis from sbt (`runMain {analysisClass}`)

## Implemented analyses

- `com.whatsapp.analyzer.OtpFunsUsage` - gathers data about which functions from
OTP libs are used in the project.
