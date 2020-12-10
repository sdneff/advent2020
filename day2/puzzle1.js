const utils = require('./utils');

function checkPassword(pwd) {
  const count = pwd.password.split('').filter(c => c === pwd.char).length;

  return count >= pwd.min
    && count <= pwd.max;
}

const input = utils.parseInput('./passwords.txt');
const validPasswords = input.filter(checkPassword);

console.log(validPasswords.length);
