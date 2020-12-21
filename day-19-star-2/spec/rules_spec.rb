require 'rules'

RSpec.describe Rules do
  let(:rules) do
    Rules.from <<~RULES.chomp
      0: 8 11
      1: "a"
      2: 1 24 | 14 4
      3: 5 14 | 16 1
      4: 1 1
      5: 1 14 | 15 1
      6: 14 14 | 1 14
      7: 14 5 | 1 21
      8: 42 | 42 8
      9: 14 27 | 1 26
      10: 23 14 | 28 1
      11: 42 31 | 42 11 31
      12: 24 14 | 19 1
      13: 14 3 | 1 12
      14: "b"
      15: 1 | 14
      16: 15 1 | 14 14
      17: 14 2 | 1 7
      18: 15 15
      19: 14 1 | 14 14
      20: 14 14 | 1 15
      21: 14 1 | 1 14
      22: 14 14
      23: 25 1 | 22 14
      24: 14 1
      25: 1 1 | 1 14
      26: 14 22 | 1 20
      27: 1 6 | 14 18
      28: 16 1
      31: 14 17 | 1 13
      42: 9 14 | 10 1
    RULES
  end

  describe '#values_for' do
    context 'when rule is simple character literal' do
      it 'returns just that character' do
        rule = rules.list.fetch(14)
        expect(rules.values_for(rule)).to eq(["b"])
      end
    end

    context 'when rule is sequence of simple character literals' do
      it 'returns the one possible value' do
        rule = rules.list.fetch(22)
        expect(rules.values_for(rule)).to eq(["bb"])
      end
    end

    context 'when rule is sequences of either one simple character or another' do
      it 'returns both possible characters' do
        rule = rules.list.fetch(15)
        expect(rules.values_for(rule)).to eq(["a", "b"])
      end
    end

    context 'when rule is a bit more involved' do
      it 'returns all the values' do
        rule = rules.list.fetch(3)
        expect(rules.values_for(rule)).to eq(["abb", "aab", "bab", "aaa", "baa", "bba"])
      end
    end
  end
end
