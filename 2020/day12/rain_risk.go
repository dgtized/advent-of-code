package main

import (
	"os"
	"strings"
	"io/ioutil"
	"fmt"
	"strconv"
	"math"
)

// usage: go run rain_risk.go input

func parseFile(file string) []string {
	input, err := ioutil.ReadFile(file)
	if err != nil {
		panic(err)
	}
	return strings.Split(string(input), "\n")
}

type Position struct {
	x float64
	y float64
	heading float64
}

func main() {
	lines := parseFile(os.Args[1])

	pos := Position{0.0,0.0,0.0}

	for i, line := range lines {
		if line == "" {break}
		dir := line[0:1]
		amount, _ := strconv.Atoi(line[1:])
		value := float64(amount)
		switch dir {
		case "N":
			pos.y += value
		case "E":
			pos.x += value
		case "S":
			pos.y -= value
		case "W":
			pos.x -= value
		case "L":
			pos.heading += value * math.Pi / 180
		case "R":
			pos.heading -= value * math.Pi / 180
		case "F":
			pos.x += value * math.Cos(pos.heading)
			pos.y += value * math.Sin(pos.heading)
		}
		fmt.Println(i, dir, value, pos)
	}

	fmt.Println("Star 1 Distance: ", math.Abs(pos.x) + math.Abs(pos.y))
}
