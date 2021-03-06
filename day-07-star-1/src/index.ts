import * as fs from "fs"

interface ChildRule {
  color: string
  quantity: number
}

interface Rule {
  color: string
  childRules: ChildRule[]
}

const rulePattern = /^([a-z ]+) bags contain (.*)\.$/
const childRulePattern = /^(\d+) ([a-z ]+) bags?$/

const parseRule = (line: string): Rule => {
  const match = line.match(rulePattern)

  if (!match) {
    throw new Error("Bad line: " + line)
  }

  let childRules: ChildRule[] = []

  if (match[2] !== "no other bags") {
    childRules = match[2].split(", ").map(
      (childRule): ChildRule => {
        const childMatch = childRule.match(childRulePattern)

        if (!childMatch) {
          throw new Error("Bad child rule: " + childRule)
        }

        return {
          color: childMatch[2],
          quantity: parseInt(childMatch[1]),
        }
      }
    )
  }

  return {
    color: match[1],
    childRules: childRules,
  }
}

const parseRules = (input: string): Rule[] => {
  return input.split("\n").map(parseRule)
}

const canContain = (color: string, rules: Rule[]): Set<string> => {
  let colors: Set<string> = new Set()

  rules.forEach((rule) => {
    if (rule.childRules.some((childRule) => childRule.color === color)) {
      colors.add(rule.color)

      canContain(rule.color, rules).forEach((canContainParentColor) => {
        colors.add(canContainParentColor)
      })
    }
  })

  return colors
}

const input: string = fs.readFileSync("./input.txt").toString().trim()

const rules: Rule[] = parseRules(input)

const canContainShinyGoldBag = canContain("shiny gold", rules)

console.log("Count", canContainShinyGoldBag.size)
