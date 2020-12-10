const fs = require('fs');

const parseInput = path => fs.readFileSync(path, 'utf-8')
    .split(/\n/)
    .map(i => {
      let match = /(\d+)-(\d+) (\S): (\S*)/.exec(i);
      return match && {
        min: parseInt(match[1]),
        max: parseInt(match[2]),
        char: match[3],
        password: match[4],
        full: i
      };
    })
    .filter(i => i != null);


exports.parseInput = parseInput;
