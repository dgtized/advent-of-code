let example = "939\n\
7,13,x,x,59,x,31,19";

let input = "1000509\n\
17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,739,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,971,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19";

function firstStar(contents) {
  let lines = contents.split("\n");
  let timestamp = parseInt(lines[0]);
  let busses =
      lines[1].
      split(",").
      filter(item => item != "x").
      map(item => parseInt(item));

  let nearest_departures = busses.map(item => item - timestamp % item);
  let nearest = Math.min(...nearest_departures);
  let idx = nearest_departures.indexOf(nearest);
  let bus = busses[idx];

  return bus * nearest;
}

console.log("** First Star");
console.log(firstStar(example),295);
console.log(firstStar(input),296);

function gcd(a, b) {
  while(b) {
    var t = b;
    b = a % b;
    a = t;
  }

  return a;
}

function lcm(a, b) {
  return a * b / gcd(a,b);
}

function solve(base, busOffset) {
  for(i = 0; i < busOffset.length; i++) {
    let bus = busOffset[i][0];
    let offset = busOffset[i][1];
    //console.log(base, bus, offset, (base + offset) % bus);
    if((base + offset) % bus != 0) {
      return false;
    }
  }
  return true;
}

function secondStar(contents) {
  let allBusses = contents.split("\n")[1].split(",");

  let busOffset = allBusses.filter(item => item != "x").
      map(item => [parseInt(item), allBusses.indexOf(item)]);

  console.log(busOffset);
  let upper = busOffset.map(item => item[0]).reduce((acc, item) => acc * item);

  let mult = busOffset[0][0];
  let base = busOffset.slice(0,-1).map(item => item[0]).reduce((acc, item) => acc * item);
  while(!solve(base, busOffset.slice(1)) && base < upper) {
    base += mult;
    if((base / mult) % 10000000 == 0) {
      console.log(base);
    }
  }

  console.log("lcm", busOffset.map(item => item[0]).reduce((acc, item) => lcm(acc, item)));
  console.log("upper", upper);

  return base;
}

console.log("** Second Star");
console.log(secondStar('1\n17,x,13,19'), 3417);
console.log(secondStar('1\n67,7,59,61'), 754018);
console.log(secondStar('1\n67,x,7,59,61'), 779210);
console.log(secondStar('1\n67,7,x,59,61'), 1261476);
console.log(secondStar('1\n1789,37,47,1889'), 1202161486);
console.log(secondStar(example), 1068788);
//console.log(secondStar(input));
