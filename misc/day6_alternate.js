const fs = require('fs');

// general utils

const chunkAt = (arr, fn) => {
  let groups = [];
  let group = [];

  function pushGroup() {
    if (group && group.length) {
      groups.push(group);
      group = [];
    }
  }

  arr.forEach(v => {
    if (fn(v)) {
      pushGroup();
    } else {
      group.push(v);
    }
  });

  pushGroup();

  return groups;
};

const countValues = (obj, fn) => {
  let count = 0;
  for (let k in obj) {
    if (fn(obj[k])) ++count;
  }
  return count;
}

// group handling utils

const parseGroup = (arr) => {
  // group = array of char Sets
  return arr.map(s => new Set(s.split('')));
}

const countQuestions = group => {
  var census = {};

  group.forEach(s =>
    s.forEach(c => {
      census[c] = (census[c] || 0) + 1;
    }));

  return {
    total: group.length,
    census: census
  };
}

// MAIN

let lines = fs.readFileSync('../day6/questionnaire.txt', 'utf-8')
  .split(/\n/);

let groupCounts = chunkAt(lines, l => !l)
  .map(parseGroup)
  .map(countQuestions);

let totalAnyAnswered = groupCounts
  .map(count => countValues(count.census, v => v > 0))
  .reduce((a, b) => a + b);

let totalAllAnswered = groupCounts
  .map(count => countValues(count.census, v => v === count.total))
  .reduce((a, b) => a + b);

console.log('sum of answers anyone in group selected is:', totalAnyAnswered)
console.log('sum of answers everyone in group selected is:', totalAllAnswered)
