#!/usr/bin/env ruby

require 'matrix'

def read_grid(filename)
  remap = { '.' => 0, '#' => 1 }
  initial = IO.readlines(filename).map(&:chomp).map do |row|
    row.scan(/./).map { |x| remap[x] }
  end
  Matrix[*initial]
end

def surroundings(i, j)
  [
    [i - 1, j], [i, j - 1], [i, j + 1], [i + 1, j]
  ].select { |x, y| (0...5).include?(x) && (0...5).include?(y) }
end

def cycle(grid)
  next_grid = grid.to_a
  grid.each_with_index do |cell, row, col|
    cells = surroundings(row, col)
    alive = cell.positive?
    adjacent = cells.count { |i, j| grid.element(i, j).positive? }
    next_grid[row][col] =
      if alive && adjacent == 1
        1
      elsif !alive && [1, 2].include?(adjacent)
        1
      else
        0
      end
  end
  Matrix[*next_grid]
end

def biodiversity(grid)
  sum = 0
  grid.each_with_index do |cell, row, col|
    if cell.positive?
      sum +=  2**(5*row + col)
    end
  end
  sum
end

grid = read_grid('input.test')
p grid
p cycle(grid)
p cycle(cycle(grid))

p biodiversity(Matrix[[0, 0, 0, 0, 0],
                      [0, 0, 0, 0, 0],
                      [0, 0, 0, 0, 0],
                      [1, 0, 0, 0, 0],
                      [0, 1, 0, 0, 0]])

grid = read_grid('input')

seen = {}

diversity = biodiversity(grid)
while true
  seen[diversity] = true
  grid = cycle(grid)
  diversity = biodiversity(grid)
  if seen.has_key?(diversity)
    break
  end
end

p diversity

