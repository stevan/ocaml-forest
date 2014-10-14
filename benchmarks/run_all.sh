#!/bin/sh

make all;
make opt;

echo "-------------------------------------------";
echo "Perl (Tree::Parser)";

perl benchmarks/benchmark.pl;

echo "";
echo "-------------------------------------------";
echo "Ocaml (bytecode)"
echo "-------------------------------------------";

ocaml benchmarks/benchmark.ml;

echo "";
echo "-------------------------------------------";
echo "OCaml (native code)";
echo "-------------------------------------------";

ocamlopt -o test -I /usr/local/lib/ocaml/site-lib/extlib extLib.cmxa forest.cmxa benchmarks/benchmark_opt.ml;
./test;
rm ./test;
rm benchmarks/benchmark_opt.cmx;
rm benchmarks/benchmark_opt.cmi;

echo "";
echo "-------------------------------------------";

make clean;