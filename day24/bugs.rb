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
  5.times do |i|
    5.times do |j|
      cells = surroundings(i, j)
      alive = grid.element(i, j).positive?
      adjacent = cells.count { |x, y| grid.element(x, y).positive? }
      next_grid[i][j] =
        if alive && adjacent == 1
          1
        elsif !alive && [1, 2].include?(adjacent)
          1
        else
          0
        end
    end
  end
  Matrix[*next_grid]
end

grid = read_grid('input.test')
p grid
p cycle(grid)
p cycle(cycle(grid))
