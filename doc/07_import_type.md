# 7. `import_type`

Erl1+ introduces a new attribute `-import_type(module, [type1, type2, ...])`
which allows using types from remote modules via short names (as if they were
defined in the current module). If the imported type is an enum, the enum
constructors can be used as the enum was defined in the current module.

## Example

From `tests/elm-core/src`.

### Without `import_type`

`maybe.erl`:

<pre>
-lang([erl2, st]).
-module(maybe).

-export_type([maybe/1]).

-export([with_default/2]).

-enum maybe(A) :: just{A} | nothing{}.

-spec with_default(A, maybe(A)) -> A.
with_default(_Default, maybe.just{Value}) -> Value;
with_default(Default, maybe.nothing{}) -> Default.

...
</pre>

`result.erl`

<pre>
-lang([erl2, st]).
-module(result).

-export_type([result/2]).
-export([to_maybe/1, from_maybe/2]).

-enum result(Error, Value) :: ok{Value} | err{Error}.

-spec to_maybe(result(_, A)) -> <b>maybe:maybe(A)</b>.
to_maybe(result.ok{V}) -> <b>maybe.maybe.just{V}</b>;
to_maybe(result.err{_}) -> <b>maybe.maybe.nothing{}</b>.

-spec from_maybe(X, <b>maybe:maybe(A))</b> -> result(X, A).
from_maybe(_Err, <b>maybe.maybe.just{V}</b>) -> result.ok{V};
from_maybe(Err, <b>maybe.maybe.nothing{}</b>) -> result.err{Err}.
</pre>

### With `import_type`

`result.erl`

<pre>
-lang([erl2, st]).
-module(result).

<b>-import_type(maybe, [maybe/1]).</b>

-export_type([result/2]).
-export([to_maybe/1, from_maybe/2]).

-enum result(Error, Value) :: ok{Value} | err{Error}.

-spec to_maybe(result(_, A)) -> <b>maybe(A)</b>.
to_maybe(result.ok{V}) -> <b>maybe.just{V}</b>;
to_maybe(result.err{_}) -> <b>maybe.nothing{}</b>.

-spec from_maybe(X, <b>maybe(A)</b>) -> result(X, A).
from_maybe(_Err, <b>maybe.just{V}</b>) -> result.ok{V};
from_maybe(Err, <b>maybe.nothing{}</b>) -> result.err{Err}.
</pre>
