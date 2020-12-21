require 'strscan'

class Rules
  CharLiteral = Struct.new(:no, :char)
  RuleSequence = Struct.new(:no, :rule_references)
  RuleSequences = Struct.new(:no, :sequences)

  def self.from(str)
    lines = str.split("\n")
    rules = []
    lines.each do |line|
      rule_no, definition = line.split(": ")
      rule_no = rule_no.to_i

      definition =
        if (match = definition.strip.match(/"(\w)"/))
          CharLiteral.new(rule_no, match[1])
        elsif (match = definition.strip.match(/^([\d\s]+)$/))
          RuleSequence.new(rule_no, match[1].split(" ").map(&:to_i))
        elsif (match = definition.strip.match(/^([\d\s\|]+)$/))
          RuleSequences.new(
            rule_no,
            match[1].split(" | ").map { |sequence|
              RuleSequence.new(nil, sequence.split(" ").map(&:to_i))
            }
          )
        else
          raise "Could not interpret definition for #{definition}"
        end

      rules[rule_no.to_i] = definition
    end

    Rules.new(rules)
  end

  def values_for(rule)
    if [0, 8, 11].include?(rule.no)
      raise "infinite values!"
    end

    case rule
    when CharLiteral
      [rule.char]
    when RuleSequence
      possibilities = rule.rule_references.map do |rule_reference|
        referenced_rule = list.fetch(rule_reference)
        values_for(referenced_rule)
      end

      first = possibilities.shift
      possibilities.reduce(first) { |acc, possibility|
        acc.product(possibility).map(&:join)
      }
    when RuleSequences
      ret = []

      rule.sequences.each do |sequence|
        values_for(sequence).each do |value|
          ret.push(value)
        end
      end

      ret
    else
      raise "Unknown rule type: #{rule}"
    end
  end

  attr_reader :list

  def initialize(list)
    @list = list
  end

  def valid?(rule_no, message)
    rule = list.fetch(rule_no)
    scanner = StringScanner.new(message)
    match?(rule, scanner) && scanner.eos?
  end

  private

  # return bool indicating if we match the rule
  #
  # advance the position only if we match
  def match?(rule, scanner)
    if rule.no == 0
      eight = list.fetch(8)
      eleven = list.fetch(11)

      match?(eight, scanner) && match?(eleven, scanner)
    elsif rule.no == 8
      match?(list.fetch(42), scanner)
    elsif rule.no == 11
      match_any_number_of?(42, scanner) &&
        match_any_number_of?(31, scanner)
    else
      values = values_for(rule)
      values.each do |value|
        if scanner.peek(value.length) == value
          scanner.pos += value.length
          return true
        end
      end
      false
    end
  end

  def match_any_number_of?(rule_no, scanner)
    count = 0

    loop do
      if match?(list.fetch(rule_no), scanner)
        count += 1
      else
        break
      end
    end

    count > 0
  end
end
