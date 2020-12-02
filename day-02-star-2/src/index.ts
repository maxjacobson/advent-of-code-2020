import * as fs from 'fs';

interface Policy {
  position: number,
  other_position: number,
  character: string,
}

type Password = string;

const pattern = /^(\d+)\-(\d+) ([a-z]): ([a-z]+)$/;

const parse = (line: string): [Policy, Password] => {
  const match: RegExpMatchArray | null = line.match(pattern);

  if (match) {
    return [
      {
        position: parseInt(match[1]),
        other_position: parseInt(match[2]),
        character: match[3],
      },
      match[4],
    ]
  } else {
    throw new Error('Bad line')
  }
}

const isValid = (policy: Policy, password: Password) => {
  let counter = 0;

  if (password[policy.position - 1] === policy.character) {
    counter++;
  }

  if (password[policy.other_position - 1] === policy.character) {
    counter++;
  }

  return counter === 1;
}

const isValidLine = (line: string): boolean => {
  let [policy, password] = parse(line);
  return isValid(policy, password);
}

(() => {
  let input: string = fs.readFileSync("./input.txt").toString();
  let counter = 0;
  input.trim().split('\n').forEach((line) => {
    if (isValidLine(line)) {
      counter++;
    }
  })

  console.log(counter);
})();
