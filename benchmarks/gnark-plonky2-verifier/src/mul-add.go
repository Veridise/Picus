package main

import (
	"github.com/Veridise/picus_gnark"
	"github.com/consensys/gnark/frontend"
	"github.com/succinctlabs/gnark-plonky2-verifier/goldilocks"
)

func main() {
	var circuit TestGoldilocksMulAddCircuit
	picus_gnark.CompilePicus("circuit", &circuit)
}

func annotateGVarIn(v goldilocks.Variable) {
	picus_gnark.CircuitVarIn(v.Limb)
}

func annotateGVarOut(v goldilocks.Variable) {
	picus_gnark.CircuitVarOut(v.Limb)
}

type TestGoldilocksMulAddCircuit struct {
	X, Y, Z        goldilocks.Variable
	ExpectedResult goldilocks.Variable
}

func (c *TestGoldilocksMulAddCircuit) Define(api frontend.API) error {
	annotateGVarIn(c.X)
	annotateGVarIn(c.Y)
	annotateGVarIn(c.Z)
	annotateGVarOut(c.ExpectedResult)
	glApi := goldilocks.New(api)
	glApi.AssertIsEqual(glApi.MulAdd(c.X, c.Y, c.Z), c.ExpectedResult)
	return nil
}
