require 'message_validator'
require 'rules'

RSpec.describe MessageValidator do
  let(:rules) do
    Rules.from <<~RULES.chomp
      42: 9 14 | 10 1
      9: 14 27 | 1 26
      10: 23 14 | 28 1
      1: "a"
      11: 42 31
      5: 1 14 | 15 1
      19: 14 1 | 14 14
      12: 24 14 | 19 1
      16: 15 1 | 14 14
      31: 14 17 | 1 13
      6: 14 14 | 1 14
      2: 1 24 | 14 4
      0: 8 11
      13: 14 3 | 1 12
      15: 1 | 14
      17: 14 2 | 1 7
      23: 25 1 | 22 14
      28: 16 1
      4: 1 1
      20: 14 14 | 1 15
      3: 5 14 | 16 1
      27: 1 6 | 14 18
      14: "b"
      21: 14 1 | 1 14
      25: 1 1 | 1 14
      22: 14 14
      8: 42
      26: 14 22 | 1 20
      18: 15 15
      7: 14 5 | 1 21
      24: 14 1
    RULES
  end

  let(:messages) do
    <<~MESSAGES.chomp.lines.map(&:chomp)
      abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
      bbabbbbaabaabba
      babbbbaabbbbbabbbbbbaabaaabaaa
      aaabbbbbbaaaabaababaabababbabaaabbababababaaa
      bbbbbbbaaaabbbbaaabbabaaa
      bbbababbbbaaaaaaaabbababaaababaabab
      ababaaaaaabaaab
      ababaaaaabbbaba
      baabbaaaabbaaaababbaababb
      abbbbabbbbaaaababbbbbbaaaababb
      aaaaabbaabaaaaababaa
      aaaabbaaaabbaaa
      aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
      babaaabbbaaabaababbaabababaaab
      aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
    MESSAGES
  end

  it 'matches the right messages' do
    valid = messages.select { |message| MessageValidator.new(rules, message).valid? }
    expect(valid).to match_array([
      'bbabbbbaabaabba',
      'babbbbaabbbbbabbbbbbaabaaabaaa',
      'aaabbbbbbaaaabaababaabababbabaaabbababababaaa',
      'bbbbbbbaaaabbbbaaabbabaaa',
      'bbbababbbbaaaaaaaabbababaaababaabab',
      'ababaaaaaabaaab',
      'ababaaaaabbbaba',
      'baabbaaaabbaaaababbaababb',
      'abbbbabbbbaaaababbbbbbaaaababb',
      'aaaaabbaabaaaaababaa',
      'aaaabbaabbaaaaaaabbbabbbaaabbaabaaa',
      'aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba',
    ])
  end

  [
    'bbabbbbaabaabba',
    'babbbbaabbbbbabbbbbbaabaaabaaa',
    'aaabbbbbbaaaabaababaabababbabaaabbababababaaa',
    'bbbbbbbaaaabbbbaaabbabaaa',
    'bbbababbbbaaaaaaaabbababaaababaabab',
    'ababaaaaaabaaab',
    'ababaaaaabbbaba',
    'baabbaaaabbaaaababbaababb',
    'abbbbabbbbaaaababbbbbbaaaababb',
    'aaaaabbaabaaaaababaa',
    'aaaabbaabbaaaaaaabbbabbbaaabbaabaaa',
    'aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba',
  ].each do |message|
    it "finds this valid #{message}" do
      expect(MessageValidator.new(rules, message)).to be_valid
    end
  end
end
