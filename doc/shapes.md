# Minimal ErlT Shapes spec

The goal of this document is describe the design of anonymous structs (now called “shapes”) for the initial release of the ErlT prototype. A key part of this proposal is that it does not replace the maps syntax which will exist side by side.

# Syntax

For anonymous structs we support four operations. Construction, pattern matching, update and indexing. In the examples 
`‘...’ `is never part of the concrete syntax it is only used to express more expressions of the same form.

### Construction:

`#(field1=FieldExpr1, ...)`

Where module fields and name are all atoms, FieldExprs can be any expression. If module is omitted the current module will be implied which goes for all of these constructs.

### Pattern Matching

`#(field1=Pattern1,...)`

Where module fields and name are all atoms, Pattern can be any Erlang pattern. This pattern matches in the open style e.g. the struct does not have to have exactly these fields, but at least these fields.

### Update

`Expr#(field1=ValueExpr1,...)`


Expr is any erlang expression, module, name and fields are all atoms, ValueExprs are any erlang expressions. If Expr does not evaluate to a struct we have a runtime error.

### Indexing

`Expr#(field)`

Expr is any erlang expression, module, name and fields are all atoms. If Expr does not evaluate to the expected struct we have a runtime error.

### Type syntax

`#(field::Type,...)`

In the type language represents an anonymous struct with exactly these fields.

`#(field::Type,..., Var)`

Represents an anonymous struct with at least these fields, and a Type variable representing the extension.



# Representation

A shape (anonymous struct) is represented as an Erlang map with atom keys.


