Red Black Tree implementation in OCaml
======================================

A new project to familar with OCaml and RBTree.

Implementation locates in `./lib/rbt.ml`.

* insertion implementation reference: <https://doi.org/10.1017/S0956796899003494>
* deletion implementation reference: <https://doi.org/10.1017/S0956796814000227>

Usage
-----

* Unit test

```console
$ nix develop -c dune runtest
...
Ran: 3 tests in: 0.10 seconds.
OK
```

* Benchmark

```console
$ nix develop -c dune exec bin/main.exe
Sequenctial insert Benchmark Result:
 0.03 WALL ( 0.02 usr +  0.01 sys =  0.03 CPU)
Randomize insert Benchmark Result:
 0.04 WALL ( 0.04 usr +  0.00 sys =  0.04 CPU)
Latencies for 100000 iterations of "Delete performance":
Delete performance:  0.04 WALL ( 0.04 usr +  0.00 sys =  0.04 CPU) @ 2767859.61/s (n=100000)
                    (warning: too few iterations for a reliable count)
                        Rate Delete performance
Delete performance 2767860/s
```
