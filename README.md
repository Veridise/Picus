<div align="left">
  <h1>
    <img src="./resources/picus-white.png" width=50>
  	Picus
  </h1>
</div>

Picus is an implementation of the $\mathsf{QED}^2$ tool, which checks the uniqueness property (under-constrained signals) of ZKP circuits.

If you are looking for the documentation for the artifact evaluation of $\mathsf{QED}^2$, please switch to the [artifact branch](https://github.com/chyanju/Picus/tree/pldi23-research-artifact).

## Getting Started Guide

This section provides basic instructions on how to test out the tool for the kick-the-tires phase. We provide pre-built docker image, which is recommended.

### Building from Docker (Recommended)

```bash
docker build -t picus:v0 .
docker run -it --memory=10g picus:v0 bash
```

> Note: you should adjust the total memory limit (10g) to a suitable value according to your machine configuration. Adding the memory restriction would prevent some benchmarks from jamming docker due to large consumption of the memory. Usually 8g memory is recommended since some benchmarks could be large.

### Building from Source

You can skip this section if you build the tool from Docker.

Building from source is not recommended if you just want to quickly run and check the results. Some dependencies require manual building and configuration, which is system specific. One only needs to make sure the following dependencies are satisfied before the tool / basic testing instructions can be executed.

#### Dependencies

- Racket (8.0+): https://racket-lang.org/
  - Rosette (4.0+): https://github.com/emina/rosette
    - `raco pkg install --auto rosette`
  - csv-reading: https://www.neilvandyke.org/racket/csv-reading/
    - `raco pkg install --auto csv-reading`
  - graph-lib: [https://pkgs.racket-lang.org/package/graph-lib](https://pkgs.racket-lang.org/package/graph-lib)
    - `raco pkg install --auto graph`
  - math-lib: [https://pkgs.racket-lang.org/package/math-lib](https://pkgs.racket-lang.org/package/math-lib)
    - `raco pkg install --auto math-lib`
- Rust: https://www.rust-lang.org/
  - for circom parser
- Node.js: https://nodejs.org/en/
  - for circom parser
- Circom (2.0.6 Required): https://docs.circom.io/
  - older version may touch buggy corner cases

- z3 solver (4.10.2+ Required): [https://github.com/Z3Prover/z3](https://github.com/Z3Prover/z3)
  - older version may touch buggy corner cases

- cvc5: [https://github.com/cvc5/cvc5](https://github.com/cvc5/cvc5) with finite field theory suport
  - see installation instructions [here](./NOTES.md#installing-cvc5-ff)

### Basic Testing Instructions

First change the directory to the repo's root path:

```bash
cd Picus/
```
Then test some benchmarks, e.g., the `Decoder` benchmark, run:

```bash
./run-picus ./benchmarks/circomlib-cff5ab6/Decoder@multiplexer.circom
```

A successful run will output logging info ***similar*** to the following (note that actual counter-example could be different due to potential stochastic search strategy in dependant libraries):

```
The circuit is underconstrained
Counterexample:
  inputs:
    main.inp: 0
  first possible outputs:
    main.out[0]: 1
    main.out[1]: 0
    main.success: 1
  second possible outputs:
    main.out[0]: 0
    main.out[1]: 0
    main.success: 0
  first internal variables:
    no first internal variables
  second internal variables:
    no second internal variables
Exiting Picus with the code 9
```

If you see this, it means the environment that you are operating on is configured successfully.

## Reusability Instructions

### More Options and APIs of the Tool

The following lists out all available options for running the tool.

```bash
usage: run-picus [ <option> ... ] <source>
  <source> must be a file with .circom or .r1cs extension

<option> is one of

  --json <json-target>
     either:
       - json logging output path; or
       - '-', which suppresses the text logging mode and
         outputs json logging to standard output
     (default: no json output)
  --noclean
     do not clean up temporary files (default: false)
  --timeout <timeout>
     timeout for SMT query (default: 5000ms)
  --solver <solver>
     solver to use: cvc4 | cvc5 | z3 (default: cvc5)
  --selector <selector>
     selector to use: counter | first (default: counter)
  --precondition <precondition>
     path to precondition json (default: none)
  --noprop
     disable propagation (default: false / propagation on)
  --nosolve
     disable solver phase (default: false / solver on)
  --strong
     check for strong safety (default: false)
  --wtns <wtns>
     wtns files output directory (default: don't output)
  --truncate <truncate>
     truncate overly long logged message: on | off (default: on)
  --log-level <log-level>
     The log-level for text logging (default: INFO)
     Possible levels (in the ascending order): DEBUG, ACCOUNTING, PROGRESS, INFO, WARNING, ERROR, CRITICAL

 circom options (only applicable for circom source)

  --opt-level <opt-level>
     optimization level for circom compilation (default: 0)

 other options

  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
```

If `<source>` is a R1CS file, we highly recommend that the Circom compilation should use the `--O0` flag and with the `--sym` option.
Otherwise, Picus may not be as effective as it can.

## Results interpretation

Picus will not output false positive. Potential outputs are:
* `safe`: Picus cannot find underconstrained bugs
* `unsafe`: Picus can find an underconstrained bug and it usually will also output the attack vector
* `unknown`: Picus cannot get a result within the given time limit. Manual review or an increase in time limit for the solver is necessary.

### z3 vs cvc5 
There are two solvers available that use different theories: `z3` and `cvc5`.
Different shapes of finite field constraints may have different difficulties for solvers with different theories. `z3` currently do not have a finite field theory, so it will not work well in majority of the cases for ZK, while `cvc5` has basic finite field support so it will work better.  

Regarding the results reported, supposing that there is no other bugs in the system and solver, if `z3` reports `safe`, it means `z3` can solve the constraints and get the result. When `cvc5` returns `unknown`, it means it got stuck in solving. In this case, the result would be `safe` since `z3` terminates with concrete results while `cvc5` cannot, supposing everything else is correct.
In case of conflicting results, `safe` for one and `unsafe` for the other, the only way to know which one is correct is to manually verify the counter example outputted by the solver that reports `unsafe`.

### Potential errors
* In case of `Killed` error: it might be a resources problem. Try to increase the memory allocated to docker.  
* In case no response / solving for a long time: this usually means the finite field solver in `cvc5` chokes. There is currently no direct solution to quickly improve the solver's performance.

### Mitigation
A common way to mitigate the above issues is to split a big circuit into smaller pieces and feed them to the tool one by one.
If a big target is too difficult to verify all at once fully automatically (which is the case for many real-world circuits), it is possible to do a semi-automatic way by manually tearing the circuits into pieces and perform automatic verification for each piece of them. If each piece can be verified successfully, this means the whole circuit is properly constrained. If all queries to the tool return `safe`, the overall result is also safe. If any query returns `unsafe` or `unknown` then the overall result is `unknown` since local unsafe cases could (but not always) lead to global vulnerability (since other parts of the circuit could have fixed the issues).

In some difficult cases where a template is very complicated -- e.g., a crypto hashing function -- and it is already difficult to verify the hash function itself, but supposing it is correct, it is possible to manually rewrite this difficult template into a relatively simple one with the same output domain to keep the computation easy for verification.   
For example, suppose there is a circuit that is composed by `f(g(h(x)))`  and `h(x)` is difficult to verify and blocking the remaining steps. But it is known that `h(x)` is definitely safe and the output domain of `h(x)` is known (let's say it is $`out_h(x)`$), it is possible to replace `h(x)` with a variable $y$ where $y \in out_h(x)$. By doing this, the semantics of the full circuit is preserved but the computation load for verification is reduced. Rewriting `h(x)` needs to be done carefully if there are more constraints applied on it, e.g., `a(b(h(x)))` -- basically the rewriting should not be breaking the semantics of the remaining part of circuit, or at least not under-approximating their relations -- that being said, if the circuit is rewritten to cover a larger variable space and can still verify it, it is ok.

Note that these two options can be used to prove the correctness of the circuit. If there is indeed a bug there, then other approaches are needed to excavate the counter-example (something like an attack vector). When these two options say a circuit is `safe` then it is definitely `safe` (given that the rewrite is correct), but when they say a circuit is `unsafe`, it may not necessarily be `unsafe` since the variable space is over-approximated-- another semi-automatic way is needed to construct the counter-example.

### Is it possible to output several counter examples if they exist ?
Yes, it is possible to output more counterexamples, but there is no user interface for now. The idea is that once a first counterexample is outputted, to get a different one, the solver can be invoked again with the first counterexample banned — it will then search for a counterexample different than the first one.


## Citations

If you find our work and this tool useful in your research, please consider citing:

```
@ARTICLE{pldi23-picus,
  title     = "Automated Detection of {Under-Constrained} Circuits in
               {Zero-Knowledge} Proofs",
  author    = "Pailoor, Shankara and Chen, Yanju and Wang, Franklyn and
               Rodr{\'\i}guez, Clara and Van Geffen, Jacob and Morton, Jason
               and Chu, Michael and Gu, Brian and Feng, Yu and Dillig, I{\c
               s}{\i}l",
  journal   = "Proc. ACM Program. Lang.",
  publisher = "Association for Computing Machinery",
  volume    =  7,
  number    = "PLDI",
  month     =  jun,
  year      =  2023,
  address   = "New York, NY, USA",
  keywords  = "SNARKs, program verification, zero-knowledge proofs"
}
```
