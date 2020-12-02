import * as fs from 'fs';

interface Policy {
  min: number,
  max: number,
  character: string,
}

type Password = string;

const pattern = /^(\d+)\-(\d+) ([a-z]): ([a-z]+)$/;

const parse = (line: string): [Policy, Password] => {
  const match: RegExpMatchArray | null = line.match(pattern);

  if (match) {
    return [
      {
        min: parseInt(match[1]),
        max: parseInt(match[2]),
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
  password.split('').forEach(character => {
    if (character === policy.character) {
      counter++;
    }
  });

  return counter >= policy.min && counter <= policy.max;
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
