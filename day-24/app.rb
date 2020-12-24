data = File.read("./input.txt")

$lobby = {}

def walk(path)
  position = [0, 0]

  path.scan(/se|ne|sw|nw|e|w/).each do |movement|
    case movement
    when 'se'
      position[0] += 1
      position[1] -= 1
    when 'ne'
      position[0] += 1
      position[1] += 1
    when 'sw'
      position[0] -= 1
      position[1] -= 1
    when 'nw'
      position[0] -= 1
      position[1] += 1
    when 'e'
      position[0] += 2
    when 'w'
      position[0] -= 2
    else
      raise
    end
  end

  $lobby[position] =
    if $lobby[position] == 'black'
      'white'
    else
      'black'
    end
end

data.lines.each do |path|
  walk path
end

# part 1
$lobby.values.count { |v| v == 'black' } # => 391

def neighbors_of(pos)
  [
    [pos[0] + 1, pos[1] - 1],
    [pos[0] + 1, pos[1] + 1],
    [pos[0] - 1, pos[1] - 1],
    [pos[0] - 1, pos[1] + 1],
    [pos[0] + 2, pos[1]],
    [pos[0] - 2, pos[1]],
  ]
end

def color_at(position)
  $lobby[position] || 'white'
end

def should_flip?(position)
  color = color_at(position)
  count = neighbors_of(position).count { |pos| color_at(pos) == 'black' }

  if color == 'white'
    count == 2
  elsif color == 'black'
    count == 0 || count > 2
  else
    raise
  end
end

def step
  should_flip = []

  $lobby.each do |position, color|
    neighbors_of(position).concat([position]).each do |pos|
      if should_flip?(pos)
        should_flip.push(pos)
      end
    end
  end

  should_flip.uniq.each do |position|
    $lobby[position] =
      if color_at(position) == 'black'
        'white'
      else
        'black'
      end
  end
end

100.times do
  step
end

# part 2
$lobby.values.count { |v| v == 'black' } # => 3876
