const fs = require('fs');

const parseInput = path => fs.readFileSync(path, 'utf-8')
  .split(/\n/)
  .map(i => parseInt(i))
  .filter(i => !isNaN(i));

const getProduct = arr => arr.reduce((a,b) => a * b);

exports.parseInput = parseInput;
exports.getProduct = getProduct;
