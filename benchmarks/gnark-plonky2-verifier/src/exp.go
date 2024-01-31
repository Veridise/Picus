package main

import (
	"math/big"

	"github.com/Veridise/picus_gnark"
	"github.com/consensys/gnark/frontend"
	"github.com/succinctlabs/gnark-plonky2-verifier/goldilocks"
)

func main() {
	var circuit TestGoldilocksExpCircuit
	picus_gnark.CompilePicus("circuit", &circuit)
}

func annotateGVarIn(v goldilocks.Variable) {
	picus_gnark.CircuitVarIn(v.Limb)
}

func annotateGVarOut(v goldilocks.Variable) {
	picus_gnark.CircuitVarOut(v.Limb)
}

type TestGoldilocksExpCircuit struct {
	X, Y goldilocks.Variable
}

func (c *TestGoldilocksExpCircuit) Define(api frontend.API) error {
	annotateGVarIn(c.X)
	annotateGVarOut(c.Y)
	glApi := goldilocks.New(api)
	var e *big.Int
	e = big.NewInt(32)
	glApi.AssertIsEqual(glApi.Exp(c.X, e), c.Y)
	return nil
}
