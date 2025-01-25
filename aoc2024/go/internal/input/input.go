package input

import (
	"fmt"
	"os"
)

func GetInput(day int) string {
	file := fmt.Sprintf("../inputs/%d.txt", day)
	dat, err := os.ReadFile(file)

	if err != nil {
		panic(err)
	}
	return string(dat)
}

func GetTestInput(day int) string {
	file := fmt.Sprintf("../input_tests/%d.txt", day)
	dat, err := os.ReadFile(file)

	if err != nil {
		panic(err)
	}
	return string(dat)
}
