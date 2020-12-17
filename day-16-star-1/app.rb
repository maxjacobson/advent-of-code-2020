class Notes
  class Rule
    def initialize(field:, ranges:)
      @field = field
      @ranges = ranges
    end

    def satisfied_by?(value)
      ranges.any? do |range|
        range.cover?(value)
      end
    end

    private

    attr_reader :ranges
  end

  def initialize(raw_notes)
    raw_rules, _my_ticket, nearby_tickets = raw_notes.split("\n\n")
    @rules = parse_rules(raw_rules)
    @nearby_tickets = parse_nearby_tickets(nearby_tickets)
  end

  def error_rate
    nearby_tickets.flatten.reduce(0) do |acc, value|
      if valid?(value)
        acc
      else
        acc + value
      end
    end
  end

  private

  attr_reader :nearby_tickets, :rules

  def valid?(value)
    rules.any? do |rule|
      rule.satisfied_by?(value)
    end
  end

  def parse_rules(raw_rules)
    raw_rules.lines.map do |line|
      match = line.strip.match(/^([\w ]+): (\d+)\-(\d+) or (\d+)\-(\d+)$/)
      raise line.inspect if match.nil?

      Rule.new(
        field: match[1],
        ranges: [
          match[2].to_i..(match[3].to_i),
          match[4].to_i..(match[5].to_i),
        ],
      )
    end
  end

  def parse_nearby_tickets(raw)
    raw.lines[1..-1].map { |line| line.chomp.split(",").map(&:to_i) }
  end
end

notes = Notes.new(File.read "./input.txt")

notes.error_rate # => 26988
