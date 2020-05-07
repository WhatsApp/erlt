-lang([erl2, st]).
-module(import_type_unused).
-compile([warn_unused_import,warnings_as_errors]).

-import_type(foo, [t/0]).
