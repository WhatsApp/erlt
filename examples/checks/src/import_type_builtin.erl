-lang([erl2, st]).
-module('import_type_builtin').
-compile(warnings_as_errors).

-import_type(foo, [integer/0]).
