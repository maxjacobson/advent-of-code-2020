require 'strscan'

OPERATORS = ["+", "*"]

def next_operand(scanner)
  scanned = []
  parens_count = 0

  scanner.scan(/\s+/)

  loop do
    char = scanner.getch

    if char == "("
      parens_count += 1
    elsif char == ")"
      parens_count -= 1
    end

    if char == " " && parens_count == 0
      break
    end

    scanned.push(char)

    break if scanner.eos?
  end

  scanned.join("")
end

def next_operator(scanner)
  loop do
    char = scanner.getch

    if OPERATORS.include?(char)
      return char
    elsif char == " "
      next
    else
      raise "expected operator, found #{char}"
    end
  end
end

def expression_without_unnecessary_parens(expression)
  expression.strip!

  if expression.start_with?("(")
    parens_count = 0
    chars = expression.split("")
    chars.each.with_index do |char, index|
      if char == "("
        parens_count += 1
      elsif char == ")"
        parens_count -= 1
      end

      if parens_count == 0 && index < expression.length - 1
        # can't remove them
        return expression
      elsif parens_count == 0 && index == expression.length - 1
        # can remove them
        return chars[1..-2].join("")
      end
    end

    raise "did not balance parens"
  else
    expression
  end
end

def expression_with_parentheses_around_plus_operations(expression)
  scanner = StringScanner.new(expression)
  tokens = []
  loop do
    tokens.push next_operand(scanner)
    scanner.scan(/\s+/)
    break if scanner.eos?
    tokens.push next_operator(scanner)
  end

  if tokens.select { |token| OPERATORS.include?(token) }.uniq.length <= 1
    return expression
  end

  first_plus_index = tokens.index("+")
  tokens.insert(first_plus_index + 2, ")")
  tokens.insert(first_plus_index - 1, "(")

  expression_with_parentheses_around_plus_operations(tokens.join(" "))
end

def evaluate(expression)
  if expression.match?(/^\s*\d+\s*$/)
    val = expression.strip.to_i
    return val
  end

  expression = expression_without_unnecessary_parens(expression)
  expression = expression_with_parentheses_around_plus_operations(expression)

  scanner = StringScanner.new(expression)
  left_operand = next_operand(scanner)
  if scanner.eos?
    return evaluate(left_operand)
  end

  operator = next_operator(scanner)

  right_operand = next_operand(scanner)

  val = evaluate(left_operand).public_send(operator, evaluate(right_operand))

  evaluate "#{val} #{scanner.rest}"
end

File.read("./input.txt").lines.reduce(0) do |acc, line|
  acc + evaluate(line.chomp)
end # => 88782789402798
