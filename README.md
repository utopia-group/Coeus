# Coeus Relational Verifier

This repository contains the verifier source code for our paper *Relational Verification using Reinforcement Learning* published at OOPSLA'19. Source code for the reinforcement learning part is released as [a separate repository](https://github.com/utopia-group/ReCoeus).

## Building from source

The Coeus verifier is written in [OCaml](https://ocaml.org). It relies on [`opam`](https://opam.ocaml.org) for package management and [`dune`](https://dune.readthedocs.io/en/stable/) for building, both of which are de-facto standard toolchain in modern OCaml development.

Download and install `opam` first by following the instructions on the opam homepage. After that, the simplest way to build and install Coeus is to leverage [opam local switch](https://opam.ocaml.org/blog/opam-local-switches/):

```bash
> git clone https://github.com/utopia-group/Coeus.git
> opam switch create ./ 4.07.1+flambda
```
This will create an `_opam` subdirectory,then build and install Coeus along with all of its dependencies from source. You'll be prompted along the way -- please answer "y". After the installation finishes, you'll get access to binary executables like `coeus`, `coeus-repl`, etc. from the same terminal.

Note that:

- The project was developed on OCaml 4.07.1. It is likely that the project itself works with a more recent compiler version, but the libraries it depends on may not.
- One of the dependencies, `z3`, takes a long time to build (usually it's 20-30 minutes on my laptop). Please be patient. 

WARNING: I also pin the version of `z3` down to 4.7.1 in the opam file, and there's a good reason for that: The spacer solver bundled with `z3-4.8` has regressed so drastically on my benchmarks that more than half of my them which can be solved by `z3-4.7.1` before would timeout on `z3-4.8`.  

For development, I don't really recommend using the installed binaries. Instead, run

```bash
> make
```
to build the project under developer mode. Built artifacts will be stored in the `_build` subdirectory. All top-level utility scripts like `run.sh`, `server.sh`, `repl.sh`, etc. will only become functional after you run `make`. 

## Command line tools

The two most important command line tool we built in the previous section are `coeus run` (in developer mode: `./run.sh`) and `coeus server` (in developer mode: `./server.sh`). 

### `coeus run`
`coeus run` takes an input file containing Coeus specification (more on this later), and then attempts to prove the problem specified in the file by applying a given proof search strategy. By default, the strategy is `random`, meaning that it will randomly apply the proof rules it knows on the file and see if the underlying CHC solver can discharge the proof obligations. You can try it on some example input files I provide under the `examples/` directory. For simple examples `random` would work surprisingly quite often --- when it doesn't work however, be prepared to Ctrl-C. 

NOTE: Pass `-v` or `--verbose` flag to let `coeus run` dump more logs to the console. Use `-v -v` if you want to see more logs and do not mind debugging output flushing your terminal.

Coeus supports other forms of search strategies that are slightly smarter than `random`. The `selfcompose` strategy does self-composition, and the `descartes` strategy is my attempt to distill and replicate the search strategy used in the PLDI'16 paper *Cartesian Hoare Logic for Verifying k-Safety Properties*. 

All strategies described so far are not exhaustive, meaning it is possible that those search algorithms may terminate without finding a proof. We do have implemented several strategies that only stop when a proof is found. For example, the `bfsexhaustive` strategy will attempt to perform a breadth-first search on applicable proof rules. The `dfsexhaustive` does depth-first search. The `randomguidedexhaustive` strategy uses Algorithm 2 mentioned in the paper except with completely random probabilities. All exhaustive strategies can be optimized by setting `--max-conflict` to a large positive number, which enables conflict analysis during proof search (i.e. give up early if we have found proof failures caused by simliar reason in the cache).

Finally, there is one special strategy called `script` that does not perform any automatic search at all. All it does is to read a manually written proof script from file (via the `--script` parameter) and replay the proof specified in the script. For example,

```bash
> coeus run examples/sample/mc91.coeus --script examples/sample/proof/mc91.script
```
If you look inside `mc91.script`, you'll find it is just a semicolon-separated list of proof rules. In short, the `script` mode provides a way to allow us quickly experiement with manual proofs.

Proof search is an extremely expensive task. For strategies other than `random`, it is recommended to carefully set restrictions on both time and machine resources to avoid the `coeus run` process going rogue. Those restrictions can all be set via either command line flags to `coeus run`, or via environment variables. Run `coeus run --help` for more details. Here's a list of setting that I like to use in my experiments:

```base
export COEUS_HOUDINI_TIMEOUT=5
export COEUS_SPACER_TIMEOUT=10
export COEUS_PRECISE_ARITH=1  # This is not recommended for ROSE benchmarks
export COEUS_DEPTH_LIMIT=20
export COEUS_TACTIC_DEPTH_LIMIT=1024
export COEUS_AST_SIZE_LIMIT=500000
```

### `coeus server`

`coeus server` takes two positional command line arguments: the first one is a directory that contains training benchmarks, and the second one is a directory that contains testing benchmarks. This command listens on a server socket (whose hostname and port can be set via `-a` and `-p`) and wait for a client to connect to it. Once the connection is established, the client can send server instructions to pick a benchmark and perform proof rollouts on it. Not surprisingly, this is how we implemented the training phase of our learning algorithm.

## Coeus specification format

Coeus implements its own AST, which consists of several function definitions, a precondition, and a postcondition. Here's an simple example of a Coeus file:

```
procedure f0(int n) returns (int r) {
  int i;
  i = 0;
  r = 0;
  while (i < n) {
    r = r + 1;
    i = i + 1;
  }
}

procedure f1(int n) returns (int r) {
  int i;
  i = n;
  r = 0;
  while (i > 0) {
    r = r + 1;
    i = i - 1;
  }
}

requires $L(n) == $R(n);
requires $L(n) >= 0;
ensures $L(r) == $R(r);
```

In this example, `f0` and `f1` are the two input functions we to the relational verifier. By default, Coeus look for function named `f0` as the left entry function and `f1` as the right entry function. If your functions have other names, you need to tell Coeus to look for them through `$lentry` and `$rentry` statement. For the function body, we support a small subset of C language. Simple function calls and recursive calls are supported, but the two top-level entry functions like `f0` and `f1` must not be invoked recursively. 

The `requires` statement speicifies the logical preconditions, and the `ensures` statement specifies the logical postconditions. If there are multiple statements, their conditions will be conjuncted. The `$L` and `$R` modifier is used to distinguish between parameters with the same name. In the example above, `$L(n)` refers to the parameter named `n` in the left entry function (which is `f0`), and `$R(n)` refers to the parameter named `n` in the right entry function (which is `f1`). Those requires and ensures basically says that if `f0` and `f1` are given the same positive integer as input, they must return the same value. 
