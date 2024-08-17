// @ts-check
const fs = require('node:fs');

/**
 * @template T
 * @param {number[]} values 
 * @param {T} defaultValue 
 */
function generateNDimensionalGrid(values, defaultValue) {
  if (values.length === 0) {
    return defaultValue;
  }

  return Array.from({ length: values[0] }, () => generateNDimensionalGrid(values.slice(1), defaultValue));
}

const NORTH = 0;
const EAST = 1;
const SOUTH = 2;
const WEST = 3;

const DIRS = [NORTH, EAST, SOUTH, WEST];

const STEP1 = 0;
const STEP2 = 1;
const STEP3 = 2;

const STEPS = [STEP1, STEP2, STEP3];

const input = fs.readFileSync('src/17_1/input.txt').toString().trim().split('\n');
const grid = input.map(line => line.split('').map(char => parseInt(char)));

const N = grid.length;

// grid.reverse();
// grid.forEach(line => line.reverse());


function gridVal(point) {
  return grid[point.x][point.y];
}

class Point {
  constructor(x, y, dir, steps) {
    this.x = x;
    this.y = y;
    this.dir = dir;
    this.steps = steps;
  }
  
  hash() {
    return `${this.x}_${this.y}_${this.dir}_${this.steps}`;
  }
}

const LAST_VALUE = gridVal(new Point(N-1, N-1))
function main() {
  /** @type {Record<string, number>} */
  const g = {};
  /** @type {Record<string, number>} */
  const f = {};

  const U = [];
  const Q = [];

  const startPoint = new Point(0, 0, SOUTH, 0);

  Q.push(startPoint);

  g[startPoint.hash()] = 0;
  f[startPoint.hash()] = estimateH(startPoint);

  let maxPoint = startPoint;

  while (Q.length) {
    const [current, currentIndex] = findMin(Q, f);

    if (maxPoint.x + maxPoint.y < current.x + current.y) {
      maxPoint = current;
      console.log(maxPoint.x, maxPoint.y, f[maxPoint.hash()])
    }

    if (current.x === N-1 && current.y === N-1) {
      break;
    }
  
    Q.splice(currentIndex, 1);
    U.push(current);

    const neighbours = getNeighbours(current);  

    for (const nbr of neighbours) {
      const tentativeScore = g[current.hash()] + gridVal(nbr);
      if (U.find(p => p.hash() === nbr.hash() && tentativeScore >= (g[nbr.hash()] ?? Infinity))) {
        continue;
      }

      if (!U.find(p => (p.hash() === nbr.hash())) || tentativeScore < (g[nbr.hash()] ?? Infinity)) {


        g[nbr.hash()] = tentativeScore;
        f[nbr.hash()] = g[nbr.hash()] + estimateH(nbr);

        if (!Q.find(p => p.hash() === nbr.hash())) {
          Q.push(nbr);
        }
      }
    }
  }
  // showGrid(g);
  // console.log(f);

  // g.reverse();
  // g.forEach(line => line.reverse());/
  console.log(f)
console.log(cross([N-1], [N-1], DIRS, STEPS).map(p => g[p.hash()]));
fs.writeFileSync('f.json', JSON.stringify(f));
  return g
}

/**
 * 
 * @param {number[]} arr1 
 * @param {number[]} arr2 
 * @param {number[]} arr3 
 * @param {number[]} arr4 
 * @returns {Point[]}
 */
function cross(arr1, arr2, arr3, arr4) {
  return arr1.flatMap(x => arr2.flatMap(y => arr3.flatMap(dir => arr4.flatMap(steps => new Point(x, y, dir, steps)))));
}

const result = main();

// fs.writeFileSync('src/17_1/estimates3.txt', showGrid(result));

/**
 * 
 * @param {Point[]} Q 
 * @param {Record<string, number>} f 
 * @returns {[Point, number]}
 */
function findMin(Q, f) {
  let minIndex = 0;
  let minPos = Q[minIndex];
  let min = f[minPos.hash()];

  for (let k = 1; k < Q.length; k++) {
    let pos = Q[k];
    let val = f[pos.hash()];

    if (val <= min) {
      minIndex = k;
      minPos = pos;
      min = val
    }
  }

  return [minPos, minIndex];
}

/**
 * 
 * @param {Point} p point 
 * @returns 
 */
function estimateH(p) {
  if (p.x === N-1 && p.y === N-1) {
    return 0;
  }

  return N - 1 - p.x + N - 1 - p.y + LAST_VALUE - 1;
}

function estimatePath(path) {
  const g = path.reduce((acc, [i, j]) => acc + grid[i][j], 0);

  const [i,j] = path[path.length - 1];
  const h = grid[N-1][N-1] + N - i + N - j - 3; // estimate

  return g + h;
}

// console.log(result);


// candidateDirections (North, (y, x)) = [(East,  (y, x + 1)), (West,  (y, x - 1)), (North, (y - 1, x))]
// candidateDirections (East,  (y, x)) = [(East,  (y, x + 1)), (South, (y + 1, x)), (North, (y - 1, x))]
// candidateDirections (South, (y, x)) = [(South, (y + 1, x)), (East,  (y, x + 1)), (West,  (y, x - 1))]
// candidateDirections (West,  (y, x)) = [(South, (y + 1, x)), (North, (y - 1, x)), (West,  (y, x - 1))]

/**
 * 
 * @param {Point} p point 
 * @returns {Point[]}
 */
function getNeighbours(p) {
  return [
    new Point(p.x, p.y + 1, EAST, p.dir === EAST ? p.steps + 1 : STEP1),
    new Point(p.x, p.y - 1, WEST, p.dir === WEST ? p.steps + 1 : STEP1),
    new Point(p.x + 1, p.y, SOUTH, p.dir === SOUTH ? p.steps + 1 : STEP1),
    new Point(p.x - 1, p.y, NORTH, p.dir === NORTH ? p.steps + 1 : STEP1),
  ].filter(p => typeof p !== 'boolean').filter(validPos);
}

/**
 * 
 * @param {Point} p point 
 * @returns {Boolean}
 */
function validPos(p) {
  return p.x >= 0 && p.x < N && p.y >= 0 && p.y < N && p.steps <= STEP3;
}

function showGrid(grid) {
  return grid.map(line => line.map(x => String(x)).join(' ')).join('\n');
}
