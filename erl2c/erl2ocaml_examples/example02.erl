-module(example02).
-compile(export_all).

odd(X) ->
  even(X).

id(X) ->
  X.

id_caller(X) ->
  id(X).

id_rec(X) ->
  id_rec(X).

even(X) ->
  odd(X).
