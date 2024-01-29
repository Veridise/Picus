// TestGoldilocksMulAddCircuit

type TestGoldilocksMulAddCircuit struct {
	X, Y, Z        goldilocks.Variable
	ExpectedResult goldilocks.Variable
}

func (c *TestGoldilocksMulAddCircuit) Define(api frontend.API) error {
	annotateGVar("in", c.X)
	annotateGVar("in", c.Y)
	annotateGVar("in", c.Z)
	annotateGVar("out", c.ExpectedResult)
	glApi := goldilocks.New(api)
	glApi.AssertIsEqual(glApi.MulAdd(c.X, c.Y, c.Z), c.ExpectedResult)
	return nil
}
