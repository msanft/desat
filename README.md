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
Feed it a CNF formular as the only argument and you're good to go.

For example:

```sh
desat '(x0 || !x1 || x2) && (!x1 || x1) && (x1)'
```

Which should yield the following output:

```console-output
SAT
Assignments:
  x1 = true
  x0 = true
```

## How to reproduce the benchmarks?

### With Nix

```sh
nix run .#benchmark
```

### Without Nix

```sh
python3 benchmark/benchmark.py
```
