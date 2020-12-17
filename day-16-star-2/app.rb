require "irb"

class Notes
  class Rule
    attr_reader :field

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
    raw_rules, my_ticket, nearby_tickets = raw_notes.split("\n\n")
    @rules = parse_rules(raw_rules)
    @my_ticket_values = parse_my_ticket(my_ticket)
    @nearby_tickets = parse_nearby_tickets(nearby_tickets).select { |ticket| valid_ticket?(ticket) }
  end

  def my_ticket
    ordered_fields.zip(my_ticket_values).each_with_object({}) do |(field, value), ticket|
      ticket[field] = value
    end
  end

  private

  attr_reader :nearby_tickets, :rules, :my_ticket_values

  def valid_ticket?(ticket_values)
    ticket_values.all? do |value|
      rules.any? do |rule|
        rule.satisfied_by?(value)
      end
    end
  end

  def ordered_fields
    fields = my_ticket_values.length.times.map { nil }

    until fields.none?(&:nil?)
      field, index = find_next_ordered_field(fields)

      fields[index] = field
    end

    fields
  end

  def find_next_ordered_field(known_fields)
    nearby_tickets.transpose.each.with_index do |column, index|
      next unless known_fields[index].nil?

      satisfied = rules.
        lazy.
        select { |rule|
          !known_fields.include?(rule.field)
        }.
        select { |rule|
        column.all? { |value|
          rule.satisfied_by?(value)
        }
      }

      if satisfied.count == 1
        return [satisfied.first.field, index]
      end
    end

    raise "Could not find anything"
  end

  def parse_rules(raw_rules)
    raw_rules.lines.map.with_index do |line, index|
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
    raw.lines[1..-1].map { |line| parse_ticket(line) }
  end

  def parse_my_ticket(raw)
    parse_ticket(raw.lines[1])
  end

  def parse_ticket(line)
    line.chomp.split(",").map(&:to_i)
  end
end

notes = Notes.new(File.read "./input.txt")
notes.my_ticket # => {"train"=>71, "arrival platform"=>127, "duration"=>181, "route"=>179, "arrival location"=>113, "departure station"=>109, "class"=>79, "price"=>151, "departure location"=>97, "row"=>107, "departure date"=>53, "zone"=>193, "arrival station"=>73, "departure platform"=>83, "seat"=>191, "type"=>101, "departure track"=>89, "arrival track"=>149, "departure time"=>103, "wagon"=>197}

notes.my_ticket.
  keep_if {|k,v| k.start_with?("departure") }.
  values.
  inject(&:*) # => 426362917709
