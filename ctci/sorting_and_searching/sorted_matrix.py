"""
Implementation for CTCI problem 9.6
"""

__all__ = ['find_sorted_matrix']
__version__ = '0.1'
__author__ = 'John Ellis'


import unittest

import numpy as np

# import ipdb


class Point(object):
    """
    Simple Point class
    """

    def __init__(self, x, y):
        super(Point, self).__init__()
        self.x = x
        self.y = y

    def __eq__(self, other):
        if not isinstance(other, Point):
            return False
        return self.x == other.x and self.y == other.y

    def __ne__(self, other):
        if not isinstance(other, Point):
            return True
        return self.x != other.x and self.y != other.y

    def __add__(self, other):
        if isinstance(other, Point):
            return Point(self.x + other.x, self.y + other.y)
        elif isinstance(other, tuple) and len(other) == 2:
            return Point(self.x + other[0], self.y + other[1])
        raise ValueError

    def __sub__(self, other):
        if isinstance(other, Point):
            return Point(self.x - other.x, self.y - other.y)
        elif isinstance(other, tuple) and len(other) == 2:
            return Point(self.x - other[0], self.y - other[1])
        raise ValueError

    def tup(self):
        return (self.x, self.y)


def find_sorted_matrix(array, value):
    """
    Find an element in a matrix where the rows and
    columns are sorted.
    """

    matrix = np.array(array)
    rows, cols = matrix.shape
    point1 = Point(0, 0)
    point2 = Point(rows - 1, cols - 1)

    while point1 != point2:
        if matrix[point1.tup()] == value:
            return point1.tup()
        elif matrix[point2.tup()] == value:
            return point2.tup()

        if matrix[point2.x, point1.y] > value:
            point2.x -= 1
        else:
            point1.y += 1

        if matrix[point1.x, point2.y] > value:
            point2.y -= 1
        else:
            point1.x += 1

    if matrix[point1.tup()] == value:
        return point1.tup()
    return None


# def __find_sorted_matrix__(matrix, value, point1, point2):
#     x1, y1 = point1
#     x2, y2 = point2
#     submatrix = matrix[x1:x2, y1:y2]
#
#     if submatrix.shape == (0, 0):
#         # Element not found
#         return None
#
#     elif submatrix.shape == (1, 1):
#         # Note; point1 == point2 also works for this check
#         if submatrix[0, 0] == value:
#             # Found element
#             return point1
#
#         # Element not found
#         return None
#
#     # Cut matrix and recurse
#     return None
#
#
# def find_sorted_matrix(array, value):
#     """
#     Find an element in a matrix where the rows and
#     columns are sorted.
#     """
#
#     matrix = np.array(array)
#     rows, cols = matrix.shape
#     point1 = (0, 0)
#     point2 = (rows - 1, cols - 1)
#     return __find_sorted_matrix__(matrix, value, point1, point2)


class TestFindSortedMatrix(unittest.TestCase):
    """
    Unit tests for find_sorted_matrix implementation.
    """

    def test1(self):
        """
        Test for simple case.
        """

        input_data = [
            [1, 2, 4, 7, 10, 15],
            [3, 5, 8, 11, 16, 22],
            [6, 9, 12, 17, 23, 27],
            [13, 14, 18, 24, 28, 31],
            [19, 20, 25, 29, 32, 34],
            [21, 26, 30, 33, 35, 36],
        ]

        to_find = 17
        expected = (2, 3)
        actual = find_sorted_matrix(input_data, to_find)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()
