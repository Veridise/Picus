// TestGoldilocksReduceCircuit

type TestGoldilocksReduceCircuit struct {
	In, Out goldilocks.Variable
}

func (c *TestGoldilocksReduceCircuit) Define(api frontend.API) error {
	glApi := goldilocks.New(api)
	annotateGVar("in", c.In)
	annotateGVar("out", c.Out)
	glApi.AssertIsEqual(glApi.Reduce(c.In), c.Out)
	return nil
}
