require 'message_validator'
require 'rules'

RSpec.describe MessageValidator do
  let(:rules) do
    Rules.from <<~RULES.chomp
      0: 4 1 5
      1: 2 3 | 3 2
      2: 4 4 | 5 5
      3: 4 5 | 5 4
      4: "a"
      5: "b"
    RULES
  end

  subject(:validator) { MessageValidator.new(rules, message) }

  describe 'ababbb' do
    let(:message) { "ababbb" }
    it { is_expected.to be_valid }
  end

  describe 'bababa' do
    let(:message) { "bababa" }
    it { is_expected.to_not be_valid }
  end
end
