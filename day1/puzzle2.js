const utils = require('./utils');

function findSumTriplet(data, sum) {
  // dynamic programming approach:
  const pool = new Set(); // track numbers encountered so far
  const pairPool = new Map(); // track pair sums encountered so far: { <sum>: [v1, v2] }

  for (const val of data) {
    const rem = sum - val;
    // if pair pool has remainder, we've hit our triplet
    if (pairPool.has(rem)) {
      return [val].concat(pairPool.get(rem));
    } else {
      // remember sums encountered so far
      pool.forEach(v => {
        pairPool.set(v + val, [v, val]);
      });
      // remember numbers encountered so far
      pool.add(val);
    }
  }

  throw new Error('no match found');
}

const input = utils.parseInput('./expense_report.txt');
const triplet = findSumTriplet(input, 2020);
const product = utils.getProduct(triplet);

console.log(product);
