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
	x int
	y int
	heading float64
}

func firstStar(lines []string) {
	pos := Position{0.0,0.0,0.0}

	for i, line := range lines {
		if line == "" {break}
		dir := line[0:1]
		value, _ := strconv.Atoi(line[1:])
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
			pos.heading += float64(value) * math.Pi / 180
		case "R":
			pos.heading -= float64(value) * math.Pi / 180
		case "F":
			pos.x += int(float64(value) * math.Cos(pos.heading))
			pos.y += int(float64(value) * math.Sin(pos.heading))
		}
		fmt.Println(i, dir, value, pos)
	}

	fmt.Println("Star 1 Distance: ", int(math.Abs(float64(pos.x)) + math.Abs(float64(pos.y))))
}

func main() {
	lines := parseFile(os.Args[1])
	firstStar(lines)
}
