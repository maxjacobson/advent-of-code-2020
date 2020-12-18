require 'set'
require 'irb'

Coordinate = Struct.new(:x, :y, :z, :w)
ACTIVE = :active
INACTIVE = :inactive

class EnergySource
  def initialize(input)
    @cubes = parse_cubes_from(input)
  end

  def step
    new_cubes = {}

    all_known_coordinates.each do |coordinate, state|
      active_neighbors = neighbors_for(coordinate).count { |_, neighbor_state| neighbor_state == ACTIVE }
      if state == ACTIVE
        if [2, 3].include?(active_neighbors)
          new_cubes[coordinate] = ACTIVE
        end
      elsif state == INACTIVE
        if active_neighbors == 3
          new_cubes[coordinate] = ACTIVE
        end
      else
        raise
      end
    end

    self.cubes = new_cubes
  end

  def active_cubes
    cubes.keys.select { |coordinate| cubes[coordinate] == ACTIVE }
  end

  private

  attr_accessor :cubes

  def all_known_coordinates
    Enumerator.new do |yielder|
      already_yielded = Set.new

      cubes.each do |coordinate, state|
        maybe_yield(yielder, already_yielded, coordinate, state)
        neighbors_for(coordinate).each do |neighbor, neighbor_state|
          maybe_yield(yielder, already_yielded, neighbor, neighbor_state)
        end
      end
    end
  end

  def maybe_yield(yielder, already_yielded, coordinate, state)
    if !already_yielded.include?(coordinate)
      yielder.yield(coordinate, state)
      already_yielded.add(coordinate)
    end
  end

  def neighbors_for(coordinate)
    Enumerator.new do |yielder|
      [coordinate.x - 1, coordinate.x, coordinate.x + 1].each do |x|
        [coordinate.y - 1, coordinate.y, coordinate.y + 1].each do |y|
          [coordinate.z - 1, coordinate.z, coordinate.z + 1].each do |z|
            [coordinate.w - 1, coordinate.w, coordinate.w + 1].each do |w|
              neighbor = Coordinate.new(x, y, z, w)

              unless neighbor == coordinate
                state = cubes[neighbor] || INACTIVE
                yielder.yield(neighbor, state)
              end
            end
          end
        end
      end
    end
  end

  def parse_cubes_from(input)
    cubes = {}

    input.lines.each.with_index do |line, x|
      line.strip.split("").each.with_index do |state, y|
        if state == '#'
          cubes[Coordinate.new(x, y, 0, 0)] = ACTIVE
        elsif state == '.'
          # Don't store the inactive one
        else
          raise
        end
      end
    end

    cubes
  end
end

input = File.read("./input.txt")
energy_source = EnergySource.new(input)

6.times do
  energy_source.step
end

puts energy_source.active_cubes.count # => 848
