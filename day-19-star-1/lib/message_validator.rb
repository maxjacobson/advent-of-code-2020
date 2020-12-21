class MessageValidator
  def initialize(rules, message)
    @rules = rules
    @message = message
  end

  def valid?
    rules.valid?(0, message)
  end

  private

  attr_reader :rules, :message
end
