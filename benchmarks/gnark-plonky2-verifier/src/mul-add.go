package main

import (
	"github.com/Veridise/picus_gnark"
	"github.com/consensys/gnark-crypto/ecc"
	"github.com/consensys/gnark/frontend"
	"github.com/succinctlabs/gnark-plonky2-verifier/goldilocks"
)

func main() {
	var circuit TestGoldilocksMulAddCircuit
	picus_gnark.CompilePicus("mul-add", &circuit, ecc.BN254.ScalarField())
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

	// technically, we should add an assumption that c.X, c.Y, and c.Z
	// are in the field
	//
	// glApi.RangeCheck(c.X)
	// glApi.RangeCheck(c.Y)
	// glApi.RangeCheck(c.Z)
	//
	// goldilocks.RangeCheck(c.X.Limb)
	// goldilocks.RangeCheck(c.Y.Limb)
	// goldilocks.RangeCheck(c.Z.Limb)
	//
	// but since it's already deemed safe even without the assumption,
	// we will leave it at that.

	glApi.AssertIsEqual(glApi.MulAdd(c.X, c.Y, c.Z), c.ExpectedResult)
	return nil
}
