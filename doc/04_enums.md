# 4. Enum syntax and semantics

* An "enum" is an algebraic data type (as they are known in Haskell), also known
  as a sum type, tagged union (C terminology), discriminated union, disjoint
  union, variant record (Pascal terminology), or simply "variant"
  (OCaml terminology)
    * Historically, in C/C++ and similar, an "enum" type was specifically
      a union type of unit types only, like `monday | tuesday | ... | sunday`,
      possibly written `Monday() | Tuesday() | ... | Sunday()`
    * Java compiles the C enum concept into a class with a number of
      constructors, and hence, it became possible to also have enums with
      constructors that take arguments. Scala extended this to ADTs with type
      parameters: `enum Foo[T] { bar(x: T), baz }`. Rust picked up this
      terminology and syntax, and used it as a general name for its
      ADT declarations.  We will probably go with this terminology for Erl2
      as well, since it is a short and memorable name for a logical
      generalization. (It does initially confuse old C programmers, though...)
* An enum `Stooges = Larry() | Curly() | Moe()`  defines:
    * A name `Stooges`  for the type itself.
      This has the same scope as other named types.
    * Names `Larry`, `Curly`, and `Moe`, for the constructors within
      the enum type. These have to be unique at least within the enum, but their
      scope differs between languages. Most languages require the enum type name
      as qualifier, so you can reuse the same constructor name within different
      enums, but Standard ML gives the constructor names module scope so that
      they can be easily referred to - this however means that different enums
      cannot reuse the same constructor names in the same module, while
      on the other hand a constructor directly identifies the enum
      it belongs to, which makes type inference easy without requiring the enum
      names as qualifiers.
    * The number of data fields (arity) of each constructor, and the types of
      these fields.
* Our current prototype implementation for Erl1+ uses the following syntax:
    * `-enum stooges() :: larry{} | curly{} | moe{}`
    * `-enum maybe(T) :: some{T} | none{}`
    * `E = maybe.some{42}`
    * `case E of maybe.some{X} -> f(X); maybe.none{} -> g() end`
    * A dotted syntax can be used to refer to a constructor in another module:
      * `E = my_mod.maybe.some{42}`
      * `case E of my_mod.maybe.some{X} -> f(X); my_mod.maybe.none{} -> g() end`
* In a statically typed language with no subtyping and no runtime type
  information, compilation of an enum simply becomes a matter of having
  a small integer as type tag to know how to interpret the rest of the fields.
  In C:

```c
struct Stooges {
    int tag;  // 0..2
    union {
        struct Larry { ...} larry;
        struct Curly { ... } curly;
        struct Moe { ... } moe;
    } u;
}
```

* In a language like ML, this is effectively what the compiler will generate
  as well, after the type checking has been resolved. Due to strict typing,
  it is known that only code that expects such a structure will ever try to
  access it, and no other runtime information is needed. It also does not
  matter what the global context of the code is, since the structs never leak
  and never can be mixed up with other data in the codebase.
* In a language where enum values could be exposed through an FFI, or examined
  via introspection via runtime type information, or serialized to be
  deserialized by someone else, the "tag" needs to preserve more information
  so that the enum and constructor can be uniquely identified. Disregarding
  efficiency, this could simply be a string: `char tag[] = "Stooges/Larry/1"`,
  and if there is a namespace system, that also needs to be part of the name:
  `"acme.com/TV/Stooges/Larry/1"`
    * Versioning of the name could also become necessary if different
      versions of the code base are expected to be able to cooperate over
      the same data.
* If Erlang had a native enum type, it would be able to hide the internals of
  the tag, but for now, we will need to use some explicit representation such as
  `{MagicEnumCookie, Module, EnumName, ConstructorName, [...]}`
    * Like Erlang’s old records, this is obviously sensitive to code upgrades
      and different versions running on different nodes, if names or arities of
      constructors change.
    * Erlang’s record definitions do not have module scope in themselves, but
      are instead shared between modules by preprocessor inclusion of header
      files. Erl2 should not be relying on such include files.
      Each enum belongs to a particular module.
    * For Erl2, we will require that if module M2 requires information about
      a type defined in module M1, then M1 must be compiled before M2.
      Note that most of the information needed by the compiler is given at
      the point where a constructor is used:
      `X = m.e.foo{true, 42}` tells us that there should be a constructor `foo`
      from enum `e` of module `m` with arity `2`. The main reasons for looking
      at the full definition of the enum are for type checking: ensuring that
      when an enum value is used, it has the correct arity, and if a variable
      has enum type, it can only contain constructors belonging to
      the same enum.
* One thing that we had to decide is how to refer to constructors.
  If we use ML style, then if module `m1` defines `insect() = bee{} | fly{}`,
  it cannot also define `verb() = run{} | fly{}`, and we would not need to use
  an enum-name qualifier when referencing them, even remotely.
  If we allow both to be defined in the same module, then local uses will have
  to say `insect.fly{}` and `verb.fly{}`, and remote uses will have to say
  `m1.insect.fly{}` and `m1.verb.fly{}`, and also `m1.insect.bee{}` and
  `m1.verb.run{}`. Locally, `bee{}` and `run{}` are unambiguous in this example,
  but could require us to add a qualifier to each use later on if the enum
  definitions are changed, e.g. by adding `bee{}` to verbs (to bee or not...),
  and this alone is probably reason not to support unqualified uses
  to begin with. We have decided to require qualifiers for all uses, and may
  add alias declarations later to allow shorter names to be used locally.
