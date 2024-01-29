// TestGoldilocksInverseCircuit

type TestGoldilocksInverseCircuit struct {
	X, Y goldilocks.Variable
	// Z    frontend.Variable
}

func (c *TestGoldilocksInverseCircuit) Define(api frontend.API) error {
	annotateGVar("in", c.X)
	annotateGVar("out", c.Y)
	// annotateGVar("out", c.Z)
	glApi := goldilocks.New(api)
	// a, b := glApi.InverseFixed(c.X)
	a := glApi.Inverse(c.X)
	glApi.AssertIsEqual(a, c.Y)
	// api.AssertIsEqual(b, c.Z)
	return nil
}
