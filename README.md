# 🐫 Desat [ˈdɛzət]

A SAT solver in OCaml.

> [!IMPORTANT]
> For reproducibility purposes, it is recommended that Desat is built / installed / tested via [Nix](https://nixos.org/).
> To install Nix on your system, use the [Determinate Systems Nix Installer](https://determinate.systems/posts/determinate-nix-installer/).

## How to build?

### With Nix

You can build Desat via:

```sh
nix build .#desat
```

You will then find the executable in `./result/bin/desat`

### Without Nix

You can build Desat via:

```sh
dune build
```

You will then find the executable in `./_build/default/bin/main.exe`

## How to use?

Desat is straight-forward to use.
Feed it a boolean formula as the only argument and you're good to go.

For example:

```sh
desat DPLL '(x0 || !x1 || x2) && (!x1 || x1) && (x1)'
```

Which should yield the following output:

```console-output
SAT
Assignments:
  x1 = true
  x0 = true
```

Desat also supports non-CNF-formulas, in which case Tseitin's transformation is used for transformation into
an equi-satisfiable CNF:

```sh
desat DPLL 'x0 <-> x1'
```

Which should yield the following output:

```console-output
Parsing as CNF failed, trying to perform Tseitin's transformation
Equi-satisfiable CNF:
t1 && (!t1 || !x0 || x1) && (!t1 || x0 || !x1) && (t1 || !x0 || !x1) && (t1 || x0 || x1)
SAT
Assignments:
  x1 = true
  x0 = true
  t1 = true
```

You can also use conflict-driven clause learning (CDCL), if you'd like. However, the current primitive implementation performs worse
than DPLL in most cases.

```sh
desat CDCL '(x0 || !x1 || x2) && (!x1 || x1) && (x1)' # Can also be non-CNF
```

## How to reproduce the benchmarks?

### With Nix

```sh
nix run .#benchmark -- <CDCL/DPLL>
```

### Without Nix

```sh
python3 benchmark/benchmark.py <CDCL/DPLL>
```
