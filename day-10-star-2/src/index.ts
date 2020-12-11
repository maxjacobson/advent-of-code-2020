import * as fs from "fs"

// from the node (key) how many paths are there to the end? (value)
interface PathCountMemo {
  [key: number]: number
}

const pathsFrom = (
  adapterVoltages: number[],
  from: number,
  to: number,
  memo: PathCountMemo
): number => {
  if (memo[from]) {
    return memo[from]
  }

  const children = adapterVoltages.filter(
    (voltage) => voltage > from && voltage <= from + 3
  )

  children.forEach((child) => {
    let n

    if (child === to) {
      n = 1
    } else {
      n = pathsFrom(adapterVoltages, child, to, memo)
    }

    memo[child] = n
  })

  return children.reduce((acc, child) => acc + memo[child], 0)
}

const adapterVoltages: number[] = fs
  .readFileSync("./input.txt")
  .toString()
  .trim()
  .split("\n")
  .map((line) => parseInt(line))
  .sort((a, b) => a - b)

const to = adapterVoltages[adapterVoltages.length - 1] + 3

adapterVoltages.push(to)

const paths = pathsFrom(adapterVoltages, 0, to, {})
console.log("paths", paths)
