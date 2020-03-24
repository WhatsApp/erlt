escript src/erl2ocaml.erl -in erl2ocaml_examples/example01.erl -out erl2ocaml_examples/example01.raw._.ml
ocaml erl2ocaml_examples/example01.raw._.ml
~/.opam/default/bin/ocamlformat --enable-outside-detected-project erl2ocaml_examples/example01.raw._.ml -o erl2ocaml_examples/example01._.ml
ocaml erl2ocaml_examples/example01._.ml
diff erl2ocaml_examples/example01._.ml erl2ocaml_examples/example01.ml

escript src/erl2ocaml.erl -in erl2ocaml_examples/example02.erl -out erl2ocaml_examples/example02.raw._.ml
ocaml erl2ocaml_examples/example02.raw._.ml
~/.opam/default/bin/ocamlformat --enable-outside-detected-project erl2ocaml_examples/example02.raw._.ml -o erl2ocaml_examples/example02._.ml
ocaml erl2ocaml_examples/example02._.ml
diff erl2ocaml_examples/example02._.ml erl2ocaml_examples/example02.ml
