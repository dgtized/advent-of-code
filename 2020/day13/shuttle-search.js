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

// derived from https://github.com/pnicorelli/nodejs-chinese-remainder/blob/master/chinese_remainder.js
// and https://rosettacode.org/wiki/Chinese_remainder_theorem#Coffeescript
function mul_inv(a, b){
  let b0 = b;
  [x0, x1] = [0, 1];
  if( b == 1 ) return 1;
  while(a > 1){
    let q = (a / b) >> 0;
    [a, b] = [b, a % b];
    [x0, x1] = [x1 - (q * x0), x0];
  }
  if( x1 < 0 )
    return x1 + b0;
  else
    return x1;
}

function chineseRemainder(a, n){
  let product = n.reduce((a,x) => a * x);
  let sum = 0;
  for(var i=0; i < n.length; i++){
    let p = product / n[i];
    sum += ( a[i] * mul_inv(p, n[i]) * p );
  }
  return sum % product;
}

function verify(base, busOffset) {
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
  let product = busOffset.map(item => item[0]).reduce((acc, item) => acc * item);
  let remainder = chineseRemainder(busOffset.map(x => x[1]), busOffset.map(x => x[0]));
  let solution = product - remainder;

  console.log(product, remainder, verify(solution, busOffset));

  return solution;
}

console.log("** Second Star");
console.log(secondStar('1\n17,x,13'), 102);
console.log(secondStar('1\n17,x,13,19'), 3417);
console.log(secondStar('1\n67,7,59,61'), 754018);
console.log(secondStar('1\n67,x,7,59,61'), 779210);
console.log(secondStar('1\n67,7,x,59,61'), 1261476);
console.log(secondStar('1\n1789,37,47,1889'), 1202161486);
console.log(secondStar(example), 1068781);
// Solution from input is off by ~13, possibly due to numeric error since every
// other example checks out. See chinese-remainder.lisp for correctly working
// numeric solution.
console.log(secondStar(input), 535296695251210);
