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
  console.log(timestamp, busses);

  nearest_departures = busses.map(item => item - timestamp % item);
  console.log(nearest_departures);

  let nearest = Math.min(...nearest_departures);
  let idx = nearest_departures.indexOf(nearest);
  let bus = busses[idx];

  return bus * nearest;
}

console.log(firstStar(example));
console.log(firstStar(input));

