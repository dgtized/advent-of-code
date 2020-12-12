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

func parseCommand(line string) (string, int) {
	dir := line[0:1]
	value, _ := strconv.Atoi(line[1:])
	return dir, value
}

type Position struct {
	x int
	y int
	heading int
}

func firstStar(lines []string) {
	pos := Position{0,0,0}

	for _, line := range lines {
		if line == "" {break}
		dir, value := parseCommand(line)
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
			pos.heading += value
		case "R":
			pos.heading -= value
		case "F":
			pos.x += int(float64(value) * math.Cos(float64(pos.heading) * math.Pi / 180))
			pos.y += int(float64(value) * math.Sin(float64(pos.heading) * math.Pi / 180))
		}

		//fmt.Println(i, dir, value, pos)
	}

	fmt.Println("Star 1 Distance: ", int(math.Abs(float64(pos.x)) + math.Abs(float64(pos.y))))
}

type Vector struct {
	x int
	y int
}

// Rotate waypoint around the spaceship using a rotation matrix
// https://en.wikipedia.org/wiki/Rotation_matrix
//
// Pretty sure there is a nice simplification here though as all angles are
// multiples of 90 degrees, so could just use fixed matrices for each direction
// and skip using cos/sin. That would also keep the rotation positions integers,
// and skip the need to account for floating point error propagation.
func rotate(degrees int, waypoint Vector) Vector {
	theta := float64(degrees) * math.Pi / 180
	x := float64(waypoint.x)
	y := float64(waypoint.y)
	return Vector{
		int(math.Round(x * math.Cos(theta) - y * math.Sin(theta))),
		int(math.Round(x * math.Sin(theta) + y * math.Cos(theta))),
	};
}

func secondStar(lines []string) {
	pos := Vector{0,0}
	waypoint := Vector{10,1}

	for i, line := range lines {
		if line == "" {break}
		dir, value := parseCommand(line)
		switch dir {
		case "N":
			waypoint.y += value
		case "E":
			waypoint.x += value
		case "S":
			waypoint.y -= value
		case "W":
			waypoint.x -= value
		case "L":
			waypoint = rotate(+value, waypoint)
		case "R":
			waypoint = rotate(-value, waypoint)
		case "F":
			pos.x += value * waypoint.x
			pos.y += value * waypoint.y
		}
		fmt.Println(i, dir, value, pos, waypoint)
	}

	fmt.Println("Star 2 Distance: ", int(math.Abs(float64(pos.x)) + math.Abs(float64(pos.y))))
}

func main() {
	lines := parseFile(os.Args[1])
	firstStar(lines)
	secondStar(lines)
}
