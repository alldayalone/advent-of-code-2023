// @ts-check
const fs = require('node:fs');

const N = 141;

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

const input = fs.readFileSync('src/17_1/input.txt').toString().trim().split('\n');
const grid = input.map(line => line.split('').map(char => parseInt(char)));

grid.reverse();
grid.forEach(line => line.reverse());

function main() {
  const g = generateNDimensionalGrid([N, N], Infinity);
  const f = generateNDimensionalGrid([N, N], Infinity);

  const U = [];
  const Q = [];

  Q.push([0, 0]);

  g[0][0] = 0;
  f[0][0] = estimateH(0, 0);

  while (Q.length) {
    const [current, currentIndex] = findMin(Q, f);

    // if (current[0] === N-1 && current[1] === N-1) {
    //   break;
    // }
  
    Q.splice(currentIndex, 1);
    U.push(current);

    const neighbours = getNeighbours(current);  

    for (const nbr of neighbours) {
      const tentativeScore = g[current[0]][current[1]] + grid[nbr[0]][nbr[1]];
      if (U.find(([i, j]) => i === nbr[0] && j === nbr[1]) && tentativeScore >= g[nbr[0]][nbr[1]]) {
        continue;
      }

      if (!U.find(([i, j]) => i === nbr[0] && j === nbr[1]) || tentativeScore < g[nbr[0][nbr[1]]]) {

        g[nbr[0]][nbr[1]] = tentativeScore;
        f[nbr[0]][nbr[1]] = g[nbr[0]][nbr[1]] + estimateH(nbr[0], nbr[1]);

        if (!Q.find(([i, j]) => i === nbr[0] && j === nbr[1])) {
          Q.push(nbr);
        }
      }
    }
  }
  // showGrid(g);
  showGrid(f);

  g.reverse();
  g.forEach(line => line.reverse());

  return g
}

const result = main();

fs.writeFileSync('src/17_1/estimates3.txt', showGrid(result));

function findMin(Q, f) {
  let minIndex = 0;
  let minPos = Q[minIndex];
  let min = f[minPos[0]][minPos[1]];

  for (let k = 1; k < Q.length; k++) {
    let pos = Q[k];
    let val = f[pos[0]][pos[1]];

    if (val < min) {
      minIndex = k;
      minPos = pos;
      min = val
    }
  }

  return [minPos, minIndex];
}

function estimateH(i, j) {
  if (i === N-1 && j === N-1) {
    return 0;
  }

  return N - 1 - i + N - 1 - j + grid[N-1][N-1] - 1;
}

function estimatePath(path) {
  const g = path.reduce((acc, [i, j]) => acc + grid[i][j], 0);

  const [i,j] = path[path.length - 1];
  const h = grid[N-1][N-1] + N - i + N - j - 3; // estimate

  return g + h;
}

console.log(result);


// candidateDirections (North, (y, x)) = [(East,  (y, x + 1)), (West,  (y, x - 1)), (North, (y - 1, x))]
// candidateDirections (East,  (y, x)) = [(East,  (y, x + 1)), (South, (y + 1, x)), (North, (y - 1, x))]
// candidateDirections (South, (y, x)) = [(South, (y + 1, x)), (East,  (y, x + 1)), (West,  (y, x - 1))]
// candidateDirections (West,  (y, x)) = [(South, (y + 1, x)), (North, (y - 1, x)), (West,  (y, x - 1))]

function getNeighbours([i, j]) {
  return [
    [i, j + 1],
    [i, j - 1],
    [i - 1, j],
    [i + 1, j],
  ].filter(validPos);
}

/**
 * 
 * @param {any[]} param0 
 * @returns 
 */
function validPos([i, j]) {
  return i >= 0 && i < N && j >= 0 && j < N;
}

function showGrid(grid) {
  return grid.map(line => line.map(x => String(x)).join(' ')).join('\n');
}
