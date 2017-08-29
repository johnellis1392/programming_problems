import unittest
from copy import deepcopy
from functools import reduce

# Write a function done_or_not passing a board (list[list_lines])
# as parameter. If the board is valid return 'Finished!',
# otherwise return 'Try again!'
#
# Sudoku rules:
#
# Complete the Sudoku puzzle so that each and every row,
# column, and region contains the numbers one through nine
# only once.
#
# Rows:
#
#
#
# There are 9 rows in a traditional Sudoku puzzle. Every row
# must contain the numbers 1, 2, 3, 4, 5, 6, 7, 8, and 9.
# There may not be any duplicate numbers in any row. In other
# words, there can not be any rows that are identical.
#
# In the illustration the numbers 5, 3, 1, and 2 are the
# "givens". They can not be changed. The remaining numbers
# in black are the numbers that you fill in to complete the row.
#
# Columns:
#
#
#
# There are 9 columns in a traditional Sudoku puzzle. Like
# the Sudoku rule for rows, every column must also contain
# the numbers 1, 2, 3, 4, 5, 6, 7, 8, and 9. Again, there may
# not be any duplicate numbers in any column. Each column will
# be unique as a result.
#
# In the illustration the numbers 7, 2, and 6 are the "givens".
# They can not be changed. You fill in the remaining numbers as
# shown in black to complete the column.
#
# Regions
#
#
#
# A region is a 3x3 box like the one shown to the left. There
# are 9 regions in a traditional Sudoku puzzle.
#
# Like the Sudoku requirements for rows and columns, every
# region must also contain the numbers 1, 2, 3, 4, 5, 6, 7, 8, and 9.
# Duplicate numbers are not permitted in any region. Each region
# will differ from the other regions.
#
# In the illustration the numbers 1, 2, and 8 are the "givens".
# They can not be changed. Fill in the remaining numbers as
# shown in black to complete the region.
#
# Valid board example:
#
#
#
# For those who don't know the game, here are some information
# about rules and how to play Sudoku:
# http://en.wikipedia.org/wiki/Sudoku
# http://www.sudokuessentials.com/


def columns(matrix):
    result = deepcopy(matrix)
    for i in range(len(matrix)):
        for j in range(len(matrix[i])):
            result[j][i] = matrix[i][j]
    return result


def validate(matrix):
    for row in matrix:
        if sorted(row) != list(range(1, 9 + 1)):
            return False
    return True


def subsections(matrix):
    result = []
    for i in range(0, 9, 3):
        for j in range(0, 9, 3):
            result.append(
                matrix[i][j:j + 3] +
                matrix[i + 1][j:j + 3] +
                matrix[i + 2][j:j + 3]
            )
    return result


class TestSubsections(unittest.TestCase):
    def setUp(self):
        self.maxDiff = None

    def test1(self):
        self.assertEqual(
            subsections([[i] * 9 for i in range(9)]),
            sorted([([i] * 3) + ([i + 1] * 3) + ([i + 2] * 3) for _ in range(3) for i in range(0, 9, 3)])
        )


def done_or_not2(board):
    valid_board = validate(board) and validate(columns(board)) and validate(subsections(board))

    return 'Finished!' if valid_board else 'Try again!'


# Holy shit numpy is awesome
import numpy as np
def done_or_not(board):
    board = np.array(board)

    rows = [board[i,:] for i in range(9)]
    columns = [board[:,j] for j in range(9)]
    subsections = [board[i:i+3,j:j+3].flatten() for i in [0,3,6] for j in [0,3,6]]

    for view in np.vstack((rows, columns, subsections)):
        if len(np.unique(view)) != 9:
            return 'Try again!'
    return 'Finished!'



class TestDoneOrNot(unittest.TestCase):
    def test1(self):
        self.assertEqual(
            done_or_not(
                [[1, 3, 2, 5, 7, 9, 4, 6, 8]
                ,[4, 9, 8, 2, 6, 1, 3, 7, 5]
                ,[7, 5, 6, 3, 8, 4, 2, 1, 9]
                ,[6, 4, 3, 1, 5, 8, 7, 9, 2]
                ,[5, 2, 1, 7, 9, 3, 8, 4, 6]
                ,[9, 8, 7, 4, 2, 6, 5, 3, 1]
                ,[2, 1, 4, 9, 3, 5, 6, 8, 7]
                ,[3, 6, 5, 8, 1, 7, 9, 2, 4]
                ,[8, 7, 9, 6, 4, 2, 1, 5, 3]]),
            'Finished!'
        )

    def test2(self):
        self.assertEqual(
            done_or_not(
                [[1, 3, 2, 5, 7, 9, 4, 6, 8]
                ,[4, 9, 8, 2, 6, 1, 3, 7, 5]
                ,[7, 5, 6, 3, 8, 4, 2, 1, 9]
                ,[6, 4, 3, 1, 5, 8, 7, 9, 2]
                ,[5, 2, 1, 7, 9, 3, 8, 4, 6]
                ,[9, 8, 7, 4, 2, 6, 5, 3, 1]
                ,[2, 1, 4, 9, 3, 5, 6, 8, 7]
                ,[3, 6, 5, 8, 1, 7, 9, 2, 4]
                ,[8, 7, 9, 6, 4, 2, 1, 3, 5]]),
            'Try again!'
        )


if __name__ == '__main__':
    unittest.main()
