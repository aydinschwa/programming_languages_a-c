# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                 rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                 [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, 2]]],
                 rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                 rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                 rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                 rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z

                 rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, 0]]), # Square with extra nub
                 [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # Extra long 
                  [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                 rotations([[0, 0], [0, -1], [1, 0]])] # V shape

                  
  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_piece_cheat (board)
    MyPiece.new([[[0, 0]]], board)
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super
    @current_block= MyPiece.next_piece(self)
    @cheat = false
    @score = 500
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.next_piece_cheat(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if (@score < 100)
      @cheat = false
    else
      @score -= 100
      @cheat = true
    end
  end
end


class MyTetris < Tetris
  # your enhancements here
  def set_board
    super
    @board = MyBoard.new(self)
  end


  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {@board.cheat})
  end
end


