# TODO - write a simple function here
escript src/erl2ocaml.erl -in erl2ocaml_examples/mod01.erl -out erl2ocaml_examples/mod01.raw._.ml
escript src/erl2ocaml.erl -in erl2ocaml_examples/mod02.erl -out erl2ocaml_examples/mod02.raw._.ml

(cd erl2ocaml_examples;
~/.opam/default/bin/ocamlformat --enable-outside-detected-project mod01.raw._.ml -o mod01._.ml;
diff mod01._.ml mod01.ml;

~/.opam/default/bin/ocamlformat --enable-outside-detected-project mod02.raw._.ml -o mod02._.ml;
diff mod02._.ml mod02.ml;

ocamlc -o mods mod01.ml mod02.ml)
