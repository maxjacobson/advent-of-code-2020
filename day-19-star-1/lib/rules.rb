require 'strscan'

class Rules
  CharLiteral = Struct.new(:char)
  RuleSequence = Struct.new(:rule_references)
  RuleSequences = Struct.new(:sequences)

  def self.from(str)
    lines = str.split("\n")
    rules = []
    lines.each do |line|
      rule_no, definition = line.split(": ")

      definition =
        if (match = definition.strip.match(/"(\w)"/))
          CharLiteral.new(match[1])
        elsif (match = definition.strip.match(/^([\d\s]+)$/))
          RuleSequence.new(match[1].split(" ").map(&:to_i))
        elsif (match = definition.strip.match(/^([\d\s\|]+)$/))
          RuleSequences.new(
            match[1].split(" | ").map { |sequence|
              RuleSequence.new(sequence.split(" ").map(&:to_i))
            }
          )
        else
          raise "Could not interpret definition for #{definition}"
        end

      rules[rule_no.to_i] = definition
    end

    Rules.new(rules)
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
    pos = scanner.pos

    matches =
      case rule
      when CharLiteral
        if scanner.peek(1) == rule.char
          scanner.getch

          true
        else
          false
        end
      when RuleSequence
        rule.rule_references.all? do |rule_reference|
          referenced_rule = list.fetch(rule_reference)
          match?(referenced_rule, scanner)
        end
      when RuleSequences
        rule.sequences.any? do |sequence|
          match?(sequence, scanner)
        end
      else
        raise "Unknown rule type: #{rule}"
      end

    unless matches
      scanner.pos = pos
    end

    matches
  end
end
