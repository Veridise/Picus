// TestGoldilocksExpCircuit

type TestGoldilocksExpCircuit struct {
	X, Y goldilocks.Variable
}

func (c *TestGoldilocksExpCircuit) Define(api frontend.API) error {
	annotateGVar("in", c.X)
	annotateGVar("out", c.Y)
	glApi := goldilocks.New(api)
	var e *big.Int
	e = big.NewInt(32)
	glApi.AssertIsEqual(glApi.Exp(c.X, e), c.Y)
	return nil
}
