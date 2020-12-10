const utils = require('./utils');

function checkPassword(pwd) {
  const pos1 = pwd.password[pwd.min - 1] === pwd.char;
  const pos2 = pwd.password[pwd.max - 1] === pwd.char;

  return pos1 !== pos2; // XOR
}

const input = utils.parseInput('./passwords.txt');
const validPasswords = input.filter(checkPassword);

console.log(validPasswords.length);
