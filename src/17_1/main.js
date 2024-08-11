// @ts-check
const fs = require('node:fs');

const N = 13;

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

function main() {
  const input = fs.readFileSync('src/17_1/input_test.txt').toString().trim().split('\n');
  const grid = input.map(line => line.split('').map(char => parseInt(char)));

  const bestGrid = generateNDimensionalGrid([N, N, 4, 3], Infinity);

  console.log(bestGrid);
  const visited = generateNDimensionalGrid([N, N, 4, 3], false);
  bestGrid[0][0][EAST][STEP1] = 0;
  bestGrid[0][0][EAST][STEP2] = 0;
  bestGrid[0][0][EAST][STEP3] = 0;

  bestGrid[0][0][SOUTH][STEP1] = 0;
  bestGrid[0][0][SOUTH][STEP2] = 0;
  bestGrid[0][0][SOUTH][STEP3] = 0;

  bestGrid[0][0][NORTH][STEP1] = 0;
  bestGrid[0][0][NORTH][STEP2] = 0;
  bestGrid[0][0][NORTH][STEP3] = 0;

  bestGrid[0][0][WEST][STEP1] = 0;
  bestGrid[0][0][WEST][STEP2] = 0;
  bestGrid[0][0][WEST][STEP3] = 0;

  /** @type {any[]} */
  const queue = [[0, 0, EAST, STEP1]];

  while (queue.length) {
    const pos = queue.shift();

    if (!pos) return;

    const [i, j, dir, step] = pos;
    const neighbours = getNeighbours(pos);
    const candidates = neighbours.filter(nbr => !visited[nbr[0]][nbr[1]][nbr[2]][nbr[3]]);

    candidates.sort((a, b) => grid[a[0]][a[1]] - grid[b[0]][b[1]]);
    candidates.forEach(nbr => {
      visited[nbr[0]][nbr[1]][nbr[2]][nbr[3]] = true;
    })

    queue.push(...candidates);
    

    for (const nbr of neighbours) {
     

      bestGrid[nbr[0]][nbr[1]][nbr[2]][nbr[3]] = Math.min(
        bestGrid[nbr[0]][nbr[1]][nbr[2]][nbr[3]],
        bestGrid[i][j][dir][pos] + grid[nbr[0]][nbr[1]]
      );

      if (Number.isNaN(bestGrid[nbr[0]][nbr[1]][nbr[2]][nbr[3]])) {
        console.log('NaN', nbr);
        throw new Error('NaN');
      }
    }
  }

  return bestGrid;
}

const result = main();

// candidateDirections (North, (y, x)) = [(East,  (y, x + 1)), (West,  (y, x - 1)), (North, (y - 1, x))]
// candidateDirections (East,  (y, x)) = [(East,  (y, x + 1)), (South, (y + 1, x)), (North, (y - 1, x))]
// candidateDirections (South, (y, x)) = [(South, (y + 1, x)), (East,  (y, x + 1)), (West,  (y, x - 1))]
// candidateDirections (West,  (y, x)) = [(South, (y + 1, x)), (North, (y - 1, x)), (West,  (y, x - 1))]

function getNeighbours([i, j, dir, step]) {
  switch (dir) {
    case NORTH:
      return [
        [i, j + 1, EAST, STEP1],
        [i, j - 1, WEST, STEP1],
        [i - 1, j, NORTH, step + 1],
      ].filter(validPos);
    case EAST:
      return [
        [i + 1, j, SOUTH, STEP1],
        [i, j + 1, EAST, step + 1],
        [i - 1, j, NORTH, STEP1],
      ].filter(validPos);
    case SOUTH:
      return [
        [i, j + 1, EAST, STEP1],
        [i + 1, j, SOUTH, step + 1],
        [i, j - 1, WEST, STEP1],
      ].filter(validPos);
    case WEST:
      return [
        [i + 1, j, SOUTH, STEP1],
        [i - 1, j, NORTH, STEP1],
        [i, j - 1, WEST, step + 1],
      ].filter(validPos);
    default:
      throw new Error('Invalid direction');
  } 
}

/**
 * 
 * @param {any[]} param0 
 * @returns 
 */
function validPos([i, j, dir, step]) {
  return i >= 0 && i < N && j >= 0 && j < N && DIRS.includes(dir) && STEPS.includes(step);
}

function showGrid(grid) {
  console.log(grid.map(line => line.map(x => String(x).padEnd(4)).join(' ')).join('\n'));
}

showGrid(result);