package v1

import (
	"testing"
)

func TestCallR(t *testing.T) {
	callR()
	t.Log(getHTMLFromResult())
}
