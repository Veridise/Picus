package main

import (
	"github.com/Veridise/picus_gnark"
	"github.com/consensys/gnark/frontend"
	"github.com/succinctlabs/gnark-plonky2-verifier/goldilocks"
)

func main() {
	var circuit TestGoldilocksReduceCircuit
	picus_gnark.CompilePicus("circuit", &circuit)
}

func annotateGVarIn(v goldilocks.Variable) {
	picus_gnark.CircuitVarIn(v.Limb)
}

func annotateGVarOut(v goldilocks.Variable) {
	picus_gnark.CircuitVarOut(v.Limb)
}

type TestGoldilocksReduceCircuit struct {
	In, Out goldilocks.Variable
}

func (c *TestGoldilocksReduceCircuit) Define(api frontend.API) error {
	glApi := goldilocks.New(api)
	annotateGVarIn(c.In)
	annotateGVarOut(c.Out)
	glApi.AssertIsEqual(glApi.Reduce(c.In), c.Out)
	return nil
}
