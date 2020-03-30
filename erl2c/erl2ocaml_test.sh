# TODO - write a simple function here
escript src/erl2ocaml.erl -erl erl2ocaml_examples/mod01.erl -ml erl2ocaml_examples/mod01.raw._.ml -mli erl2ocaml_examples/mod01.raw._.mli
escript src/erl2ocaml.erl -erl erl2ocaml_examples/mod02.erl -ml erl2ocaml_examples/mod02.raw._.ml -mli erl2ocaml_examples/mod02.raw._.mli

(cd erl2ocaml_examples;
~/.opam/default/bin/ocamlformat --enable-outside-detected-project mod01.raw._.mli -o mod01._.mli;
diff mod01._.mli mod01.mli;
~/.opam/default/bin/ocamlformat --enable-outside-detected-project mod01.raw._.ml -o mod01._.ml;
diff mod01._.ml mod01.ml;

~/.opam/default/bin/ocamlformat --enable-outside-detected-project mod02.raw._.mli -o mod02._.mli;
diff mod02._.mli mod02.mli;
~/.opam/default/bin/ocamlformat --enable-outside-detected-project mod02.raw._.ml -o mod02._.ml;
diff mod02._.ml mod02.ml;

ocamlc -c Ffi.ml mod01.mli mod01.ml mod02.mli mod02.ml
)
