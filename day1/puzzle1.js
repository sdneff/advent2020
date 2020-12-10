const utils = require('./utils');

function findSumPair(data, sum) {
  // dynamic programming approach:
  const pool = new Set(); // track numbers encountered so far

  for (const val of data) {
    const rem = sum - val;
    // once we find our remainder, we've got our pair
    if (pool.has(rem)) {
      return [rem, val];
    } else {
      pool.add(val);
    }
  }

  throw new Error('no match found');
}

const input = utils.parseInput('./expense_report.txt');
const pair = findSumPair(input, 2020);
const product = utils.getProduct(pair);

console.log(product);
