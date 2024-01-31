package main

import (
	"github.com/Veridise/picus_gnark"
	"github.com/consensys/gnark/frontend"
	"github.com/succinctlabs/gnark-plonky2-verifier/goldilocks"
)

func main() {
	var circuit TestGoldilocksInverseCircuit
	picus_gnark.CompilePicus("circuit", &circuit)
}

func annotateGVarIn(v goldilocks.Variable) {
	picus_gnark.CircuitVarIn(v.Limb)
}

func annotateGVarOut(v goldilocks.Variable) {
	picus_gnark.CircuitVarOut(v.Limb)
}

type TestGoldilocksInverseCircuit struct {
	X, Y goldilocks.Variable
}

func (c *TestGoldilocksInverseCircuit) Define(api frontend.API) error {
	annotateGVarIn(c.X)
	annotateGVarOut(c.Y)
	glApi := goldilocks.New(api)

	glApi.RangeCheck(c.X)
	// goldilocks.RangeCheck(c.X.Limb)

	glApi.AssertIsEqual(glApi.Inverse(c.X), c.Y)
	return nil
}
