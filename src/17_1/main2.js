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

function printDir(dir) {
  switch (dir) {
    case NORTH: return 'NORTH';
    case EAST: return 'EAST';
    case SOUTH: return 'SOUTH';
    case WEST: return 'WEST';
  }
}

const input = fs.readFileSync('src/17_1/input_test.txt').toString().trim().split('\n');
const grid = input.map(line => line.split('').map(char => parseInt(char)));

const N = grid.length;

// grid.reverse();
// grid.forEach(line => line.reverse());


function gridVal(point) {
  return grid[point.x][point.y];
}

class Point {
  constructor(x, y, dir) {
    this.x = x;
    this.y = y;
    this.dir = dir;
  }
  
  hash() {
    return `${this.x}_${this.y}_${this.dir}`;
  }
}

const LAST_VALUE = gridVal(new Point(N-1, N-1))
function main() {
  console.log("STARTING")
  /** @type {Record<string, number>} */
  const g = {};
  /** @type {Record<string, Point[]>} */
  const bestPaths = {};
  /** @type {Record<string, number>} */
  const f = {};
  /** @type {Record<string, boolean>} */
  const U = {};
  const Q = [];

  const startPoint = new Point(0, 0, NORTH);
  const startPoint2 = new Point(0, 0, WEST);

  Q.push(startPoint, startPoint2);

  bestPaths[startPoint.hash()] = [startPoint];
  g[startPoint.hash()] = 0;
  f[startPoint.hash()] = estimateH(startPoint);

  bestPaths[startPoint2.hash()] = [startPoint2];
  g[startPoint2.hash()] = 0;
  f[startPoint2.hash()] = estimateH(startPoint2);

  let maxPoint = startPoint;

  while (Q.length) {
    const [current, currentIndex] = findMin(Q, f);

    // console.log(current.x, current.y, current.dir, g[current.hash()])
    if (maxPoint.x + maxPoint.y < current.x + current.y) {
      maxPoint = current;
      console.log(maxPoint.x, maxPoint.y, f[maxPoint.hash()])
    }

    if (current.x === N-1 && current.y === N-1) {
      break;
    }
  
    Q.splice(currentIndex, 1);
    U[current.hash()] = true;

    const neighbours = getNeighbours(current);  

    for (const [nbr, val] of neighbours) {
      const tentativeScore = g[current.hash()] + val;
      if (U[nbr.hash()] && tentativeScore >= (g[nbr.hash()] ?? Infinity)) {
        continue;
      }

      if (!Q.find(p => (p.hash() === nbr.hash())) || tentativeScore < (g[nbr.hash()] ?? Infinity)) {


        g[nbr.hash()] = tentativeScore;
        f[nbr.hash()] = g[nbr.hash()] + estimateH(nbr);
        bestPaths[nbr.hash()] = bestPaths[current.hash()].concat(nbr);

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
  console.log(g)
console.log(cross([N-1], [N-1], DIRS).map(p => g[p.hash()]));
console.log(cross([N-1], [N-1], DIRS).map(p => bestPaths[p.hash()]));

fs.writeFileSync('f.json', JSON.stringify(f));
  return g
}

/**
 * 
 * @param {number[]} arr1 
 * @param {number[]} arr2 
 * @param {number[]} arr3 
 * @returns {Point[]}
 */
function cross(arr1, arr2, arr3) {
  return arr1.flatMap(x => arr2.flatMap(y => arr3.flatMap(dir => new Point(x, y, dir))));
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

    if (val < min) {
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
 * @returns {[Point, number][]}
 */
function getNeighbours(p) {
  return [
    p.dir !== EAST && p.dir !== WEST && [
      new Point(p.x, p.y + 1, EAST),
      new Point(p.x, p.y + 2, EAST),
      new Point(p.x, p.y + 3, EAST),
    ],
    p.dir !== WEST && p.dir !== EAST && [
    new Point(p.x, p.y - 1, WEST),
    new Point(p.x, p.y - 2, WEST),
    new Point(p.x, p.y - 3, WEST),
    ],
    p.dir !== SOUTH &&  p.dir !== NORTH && [
      new Point(p.x + 1, p.y, SOUTH),
      new Point(p.x + 2, p.y, SOUTH),
      new Point(p.x + 3, p.y, SOUTH),
    ],
    p.dir !== NORTH &&  p.dir !== SOUTH && [
      new Point(p.x - 1, p.y, NORTH),
      new Point(p.x - 2, p.y, NORTH),
      new Point(p.x - 3, p.y, NORTH),
    ]
  ]
  .filter(p => typeof p !== 'boolean')
  .map(pts => pts.filter(validPos))
  .flatMap(wrapVals)
}

/**
 * 
 * @param {Point[]} pts 
 * @returns {[Point, number][]}
 */
function wrapVals(pts) {
  let val = 0;
  let wrapped = []

  for (const p of pts) {
    val += gridVal(p);
    wrapped.push(/** @type {[Point, number]} */ ([p, val]));
  }

  return wrapped;
}

/**
 * 
 * @param {Point} p point 
 * @returns {Boolean}
 */
function validPos(p) {
  return p.x >= 0 && p.x < N && p.y >= 0 && p.y < N;
}

function showGrid(grid) {
  return grid.map(line => line.map(x => String(x)).join(' ')).join('\n');
}
