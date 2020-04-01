set -x
set -e

(cd ..; make;)

for m in "mod01" "mod02" "mod03" "mod04"
do
    ./bin/erl2ocaml -erl "erl2ocaml_examples/${m}.erl" -ml "erl2ocaml_examples/${m}.raw._.ml" -mli "erl2ocaml_examples/${m}.raw._.mli"
done

(cd erl2ocaml_examples;

rm -f *.cmi *.cmo
ocamlc -c Ffi.ml;

for m in "mod01" "mod02" "mod03" "mod04"
do
    ocamlformat --enable-outside-detected-project "${m}.raw._.mli" -o "${m}._.mli";
    diff "${m}._.mli" "${m}.mli";
    ocamlformat --enable-outside-detected-project "${m}.raw._.ml" -o "${m}._.ml";
    diff "${m}._.ml" "${m}.ml";
    ocamlc -c "${m}.mli" "${m}.ml";
done
rm -f *.cmi *.cmo;
)

echo "OK"
