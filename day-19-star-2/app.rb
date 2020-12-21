$LOAD_PATH << "./lib"

require 'message_validator'
require 'rules'

input = File.read("./input.txt")
rules, messages = input.split("\n\n")
rules = Rules.from(rules)
messages = messages.strip.lines.map(&:chomp)

messages.count { |message|
  MessageValidator.new(rules, message).valid?
} # => 379
