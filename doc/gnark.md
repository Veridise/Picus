# gnark support

Picus supports gnark, but it requires users to manually annotate some metadata to extract constraints into 
a format that we call `sr1cs`. This documentation details the constraint extraction.

## Step-by-step instructions

There are two steps to extract constraints into the `sr1cs` format:

### Step 1: entrypoint file

Create an entry point `picus.go` with the following content:

```go
package main

import (
	"fmt"
	"os"

	"github.com/consensys/gnark-crypto/ecc"
	"github.com/consensys/gnark/constraint"
	"github.com/consensys/gnark/frontend"
	"github.com/consensys/gnark/frontend/cs/r1cs"
)

var fInfo *os.File

func picus(r1cs constraint.ConstraintSystem) {
	fmt.Fprintf(fInfo, "(num-wires %v)\n", r1cs.GetNbSecretVariables()+r1cs.GetNbPublicVariables()+r1cs.GetNbInternalVariables())
	fmt.Fprintf(fInfo, "(prime-number %v)\n", r1cs.Field())

	nR1CS, ok := r1cs.(constraint.R1CS)
	if ok {
		constraints := nR1CS.GetR1Cs()
		for _, r1c := range constraints {
			fmt.Fprintf(fInfo, "(constraint ")
			fmt.Fprintf(fInfo, "[")

			for i := 0; i < len(r1c.L); i++ {
				fmt.Fprintf(fInfo, "(%v %v) ", r1cs.CoeffToString(int(r1c.L[i].CID)), r1c.L[i].VID)
			}
			fmt.Fprintf(fInfo, "] [")
			for i := 0; i < len(r1c.R); i++ {
				fmt.Fprintf(fInfo, "(%v %v) ", r1cs.CoeffToString(int(r1c.R[i].CID)), r1c.R[i].VID)
			}
			fmt.Fprintf(fInfo, "] [")
			for i := 0; i < len(r1c.O); i++ {
				fmt.Fprintf(fInfo, "(%v %v) ", r1cs.CoeffToString(int(r1c.O[i].CID)), r1c.O[i].VID)
			}
			fmt.Fprintf(fInfo, "])\n")
		}
	}
}

func annotateVar(kind string, v frontend.Variable) {
	fmt.Fprintf(fInfo, "(%v %v)\n", kind, v)
}

func extraConstraint(cnst string) {
	fmt.Fprintf(fInfo, "(extra-constraint %v)\n", cnst)
}

func main() {
	fTmp, _ := os.Create("circuit.sr1cs")
	fInfo = fTmp
	defer fInfo.Close()

	var circuit <PUT-THE-CIRCUIT-TYPE-HERE>

	r1cs, _ := frontend.Compile(ecc.BN254.ScalarField(), r1cs.NewBuilder, &circuit)
	picus(r1cs)
}

// circuit-specific details start here

type <PUT-THE-CIRCUIT-TYPE-HERE> struct {
	...
}

func ... Define(api frontend.API) error {
	...
}
```

where `<PUT-THE-CIRCUIT-TYPE-HERE>` should be replaced with the circuit type that we wish to verify, and the section after 
`// circuit-specific details start here` should be replaced with the circuit implementation.

### Step 2: annotate inputs and outputs

Use the function `annotateVar` with either `"in"` or `"out"` as the first argument, and with 
a `frontend.Variable` as the second argument, to annotate that the variable should be treated as an input/output.

## Examples

Let's say that we want to verify that the following `MyCircuit` circuit from the [gnark tutorial](https://docs.gnark.consensys.io/HowTo/write/circuit_api)
is properly constrained.

```go
type MyCircuit struct {
	X, Y frontend.Variable
}

func (circuit *MyCircuit) Define(api frontend.API) error {
	x3 := api.Mul(circuit.X, circuit.X, circuit.X)
	api.AssertIsEqual(circuit.Y, api.Add(x3, circuit.X, 5))
	return nil
}
```

We create the entry point file, replace `<PUT-THE-CIRCUIT-TYPE-HERE>` with `MyCircuit`, 
and put the above circuit implementation at the end.

Next, we annotate the input and output variables by making the following modification:


```go
func (circuit *MyCircuit) Define(api frontend.API) error {
    annotateVar("in", circuit.X)
    annotateVar("out", circuit.Y)
	x3 := api.Mul(circuit.X, circuit.X, circuit.X)
	api.AssertIsEqual(circuit.Y, api.Add(x3, circuit.X, 5))
	return nil
}
```

The full content of `picus.go` is now as follows:

```go
package main

import (
	"fmt"
	"os"

	"github.com/consensys/gnark-crypto/ecc"
	"github.com/consensys/gnark/constraint"
	"github.com/consensys/gnark/frontend"
	"github.com/consensys/gnark/frontend/cs/r1cs"
)

var fInfo *os.File

func picus(r1cs constraint.ConstraintSystem) {
	fmt.Fprintf(fInfo, "(num-wires %v)\n", r1cs.GetNbSecretVariables()+r1cs.GetNbPublicVariables()+r1cs.GetNbInternalVariables())
	fmt.Fprintf(fInfo, "(prime-number %v)\n", r1cs.Field())

	nR1CS, ok := r1cs.(constraint.R1CS)
	if ok {
		constraints := nR1CS.GetR1Cs()
		for _, r1c := range constraints {
			fmt.Fprintf(fInfo, "(constraint ")
			fmt.Fprintf(fInfo, "[")

			for i := 0; i < len(r1c.L); i++ {
				fmt.Fprintf(fInfo, "(%v %v) ", r1cs.CoeffToString(int(r1c.L[i].CID)), r1c.L[i].VID)
			}
			fmt.Fprintf(fInfo, "] [")
			for i := 0; i < len(r1c.R); i++ {
				fmt.Fprintf(fInfo, "(%v %v) ", r1cs.CoeffToString(int(r1c.R[i].CID)), r1c.R[i].VID)
			}
			fmt.Fprintf(fInfo, "] [")
			for i := 0; i < len(r1c.O); i++ {
				fmt.Fprintf(fInfo, "(%v %v) ", r1cs.CoeffToString(int(r1c.O[i].CID)), r1c.O[i].VID)
			}
			fmt.Fprintf(fInfo, "])\n")
		}
	}
}

func annotateVar(kind string, v frontend.Variable) {
	fmt.Fprintf(fInfo, "(%v %v)\n", kind, v)
}

func extraConstraint(cnst string) {
	fmt.Fprintf(fInfo, "(extra-constraint %v)\n", cnst)
}

func main() {
	fTmp, _ := os.Create("circuit.sr1cs")
	fInfo = fTmp
	defer fInfo.Close()

	var circuit MyCircuit

	r1cs, _ := frontend.Compile(ecc.BN254.ScalarField(), r1cs.NewBuilder, &circuit)
	picus(r1cs)
}

type MyCircuit struct {
	X, Y frontend.Variable
}

func (circuit *MyCircuit) Define(api frontend.API) error {
	annotateVar("in", circuit.X)
	annotateVar("out", circuit.Y)
	x3 := api.Mul(circuit.X, circuit.X, circuit.X)
	api.AssertIsEqual(circuit.Y, api.Add(x3, circuit.X, 5))
	return nil
}
```

Running `go run picus.go` should produce the following result:

```
12:27:59 INF compiling circuit
12:27:59 INF parsed circuit inputs nbPublic=0 nbSecret=2
12:27:59 INF building constraint builder nbConstraints=3
```

along with the `circuit.sr1cs` file:

```
(in [{1 [12436184717236109307 3962172157175319849 7381016538464732718 1011752739694698287 0 0]}])
(out [{2 [12436184717236109307 3962172157175319849 7381016538464732718 1011752739694698287 0 0]}])
(num-wires 5)
(prime-number 21888242871839275222246405745257275088548364400416034343698204186575808495617)
(constraint [(1 1) ] [(1 1) ] [(1 3) ])
(constraint [(1 3) ] [(1 1) ] [(1 4) ])
(constraint [(1 0) ] [(1 2) ] [(5 0) (1 1) (1 4) ])
```

This `circuit.sr1cs` can be used with Picus directly. Running `/path/to/run-picus circuit.sr1cs` produces the following result:

```
The circuit is properly constrained
Exiting Picus with the code 8
```

On the other hand, if we verify the following circuit instead:

```go
func (circuit *MyCircuit) Define(api frontend.API) error {
    annotateVar("in", circuit.X)
    annotateVar("out", circuit.Y)
	api.AssertIsEqual(api.Mul(circuit.Y, circuit.Y), circuit.X)
	return nil
}
```

We would obtain an `sr1cs` file that is not properly constrained.

```bash
$ go run picus.go
$ /path/to/run-picus circuit.sr1cs 
working directory: <elided>
The circuit is underconstrained
Counterexample:
  inputs:
    1: 1
  first possible outputs:
    2: 1
  second possible outputs:
    2: 21888242871839275222246405745257275088548364400416034343698204186575808495616
  first internal variables:
    3: 1
  second internal variables:
    3: 1
Exiting Picus with the code 9
```
