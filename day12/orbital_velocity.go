package main

import (
	"crypto/sha256"
	"fmt"
)

type body struct {
	x  int
	y  int
	z  int
	dx int
	dy int
	dz int
}

var body_count = 4;

func delta_v(pa *int, pb *int, da *int, db *int) {
	if *pa > *pb {
		(*da)--
		(*db)++
	} else if *pa < *pb {
		(*da)++
		(*db)--
	}
}

func step(bodies *([]body)) {
	for outer := 0; outer < body_count; outer++ {
		for inner := outer + 1; inner < body_count; inner++ {
			delta_v(&(*bodies)[outer].x, &(*bodies)[inner].x,
				&(*bodies)[outer].dx, &(*bodies)[inner].dx)
			delta_v(&(*bodies)[outer].y, &(*bodies)[inner].y,
				&(*bodies)[outer].dy, &(*bodies)[inner].dy)
			delta_v(&(*bodies)[outer].z, &(*bodies)[inner].z,
				&(*bodies)[outer].dz, &(*bodies)[inner].dz)
		}
	}

	for i := 0; i < body_count; i++ {
		(*bodies)[i].x += (*bodies)[i].dx
		(*bodies)[i].y += (*bodies)[i].dy
		(*bodies)[i].z += (*bodies)[i].dz
	}
}

func abs(v int) int {
	if v < 0 {
		return -v
	} else {
		return v
	}
}

func energy(bodies []body) int {
	var total int
	for i := 0; i < body_count; i++ {
		var potential = abs(bodies[i].x) + abs(bodies[i].y) + abs(bodies[i].z)
		var kinetic = abs(bodies[i].dx) + abs(bodies[i].dy) + abs(bodies[i].dz)
		total += potential * kinetic
	}
	return total
}

func run_sim(bodies []body, iterations int, print bool) {
	fmt.Println(0, bodies)
	for iter := 0; iter < iterations; iter++ {
		step(&bodies)
		if print {
			fmt.Println(iter+1, bodies)
		}
	}
	fmt.Println("**", bodies)
	fmt.Println(energy(bodies))
}

func hash(bodies []body) string {
	h := sha256.New()
	h.Write([]byte(fmt.Sprintf("%v", bodies)))
	return fmt.Sprintf("%x", h.Sum(nil))
}

func find_repeat(bodies []body) {
	var seen = make(map[string]int)

	var iter int
	for {
		step(&bodies)
		var h = hash(bodies)
		var last, found = seen[h]
		if found {
			println("seen @", iter, " last", last, " steps")
			break
		} else {
			seen[h] = iter
		}
		iter++
	}
	fmt.Println(bodies)
}

func main() {
	var test = []body{
		body{-1, 0, 2, 0, 0, 0},
		body{2, -10, -7, 0, 0, 0},
		body{4, -8, 8, 0, 0, 0},
		body{3, 5, -1, 0, 0, 0},
	}
	var test2 = []body{
		body{-8, -10, 0, 0, 0, 0},
		body{5, 5, 10, 0, 0, 0},
		body{2, -7, 3, 0, 0, 0},
		body{9, -8, -3, 0, 0, 0},
	}
	var bodies = []body{
		body{16, -11, 2, 0, 0, 0},
		body{0, -4, 7, 0, 0, 0},
		body{6, 4, -10, 0, 0, 0},
		body{-3, -2, -4, 0, 0, 0},
	}

	run_sim(test, 10, true)
	run_sim(test2, 100, false)
	run_sim(bodies, 1000, false)

	find_repeat(test)
	find_repeat(bodies)
}
