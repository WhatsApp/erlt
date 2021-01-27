# Experimental Checker for Core Erlang

in a shell, in this directory:

```sh
erlc +debug_info core_analyzer.erl && erl -sname core_analyzer@localhost -pa
```

in another shell in this directory:

```sh
sbt test
```

NOTE: I hardcoded an absolute path to my file system somewhere, search for "mheiber" and fix it, then teach me how to Java
