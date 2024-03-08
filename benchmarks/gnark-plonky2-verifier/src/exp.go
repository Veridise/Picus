package main

import (
	"math/big"

	"github.com/Veridise/picus_gnark"
	"github.com/consensys/gnark-crypto/ecc"
	"github.com/consensys/gnark/frontend"
	"github.com/succinctlabs/gnark-plonky2-verifier/goldilocks"
)

func main() {
	var circuit TestGoldilocksExpCircuit
	picus_gnark.CompilePicus("exp", &circuit, ecc.BN254.ScalarField())
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

	// technically, we should add an assumption that c.X is in the field
	//
	// glApi.RangeCheck(c.X)
	// goldilocks.RangeCheck(c.X.Limb)
	//
	// but since it's already deemed safe even without the assumption,
	// we will leave it at that.

	glApi.AssertIsEqual(glApi.Exp(c.X, big.NewInt(32)), c.Y)
	return nil
}
