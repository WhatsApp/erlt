set -e

(cd ..; make;)

# TODO - write a simple boilerplate function here
./bin/erl2ocaml -erl erl2ocaml_examples/mod01.erl -ml erl2ocaml_examples/mod01.raw._.ml -mli erl2ocaml_examples/mod01.raw._.mli
./bin/erl2ocaml -erl erl2ocaml_examples/mod02.erl -ml erl2ocaml_examples/mod02.raw._.ml -mli erl2ocaml_examples/mod02.raw._.mli
./bin/erl2ocaml -erl erl2ocaml_examples/mod03.erl -ml erl2ocaml_examples/mod03.raw._.ml -mli erl2ocaml_examples/mod03.raw._.mli
./bin/erl2ocaml -erl erl2ocaml_examples/mod04.erl -ml erl2ocaml_examples/mod04.raw._.ml -mli erl2ocaml_examples/mod04.raw._.mli

(cd erl2ocaml_examples;

ocamlformat --enable-outside-detected-project mod01.raw._.mli -o mod01._.mli;
diff mod01._.mli mod01.mli;
ocamlformat --enable-outside-detected-project mod01.raw._.ml -o mod01._.ml;
diff mod01._.ml mod01.ml;

ocamlformat --enable-outside-detected-project mod02.raw._.mli -o mod02._.mli;
diff mod02._.mli mod02.mli;
ocamlformat --enable-outside-detected-project mod02.raw._.ml -o mod02._.ml;
diff mod02._.ml mod02.ml;

ocamlformat --enable-outside-detected-project mod03.raw._.mli -o mod03._.mli;
diff mod03._.mli mod03.mli;
ocamlformat --enable-outside-detected-project mod03.raw._.ml -o mod03._.ml;
diff mod03._.ml mod03.ml;

ocamlformat --enable-outside-detected-project mod04.raw._.mli -o mod04._.mli;
diff mod04._.mli mod04.mli;
ocamlformat --enable-outside-detected-project mod04.raw._.ml -o mod04._.ml;
diff mod04._.ml mod04.ml;

ocamlc -c Ffi.ml mod01.mli mod01.ml mod02.mli mod02.ml mod03.mli mod03.ml mod04.mli mod04.ml;)

echo "OK"
