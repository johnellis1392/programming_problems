import unittest
# from copy import deepcopy
import numpy as np

# IPython Debugger Documentation:
# https://github.com/gotcha/ipdb
from ipdb import set_trace as debug


def matrix_swap(matrix, p1, p2):
    """
    Swap two elements in a matrix.
    * matrix: numpy.array
    * x1
    * y1
    * x2
    * y2
    """

    x1, y1 = p1
    x2, y2 = p2

    # print("Swapping Elements: matrix[%d, %d] == %d, and matrix[%d, %d] == %d" % (x1, y1, matrix[x1, y1], x2, y2, matrix[x2, y2]))
    i = matrix[x1, y1]
    matrix[x1, y1] = matrix[x2, y2]
    matrix[x2, y2] = i

    # print("Matrix:\n%s" % str(matrix))
    # print()
    return matrix


#@unittest.skip("fizzbuzz")
class TestMatrixSwap(unittest.TestCase):

    def assert_matrix_swap(self, expected, input_data, p1, p2):
        actual = matrix_swap(input_data, p1, p2)
        self.assertEqual(actual.tolist(), expected.tolist())


    def test1(self):
        p1 = 0, 0
        p2 = 0, 1
        input_data = np.array([
            [1, 0],
            [0, 1]
        ])

        expected = np.array([
            [0, 1],
            [0, 1]
        ])

        self.assert_matrix_swap(expected, input_data, p1, p2)


    def test2(self):
        p1 = 0, 0
        p2 = 0, 1
        input_data = np.array([
            [1, 2],
            [3, 4]
        ])

        expected = np.array([
            [2, 1],
            [3, 4]
        ])

        self.assert_matrix_swap(expected, input_data, p1, p2)



def matrix_rotation(input_data):
    matrix = np.array(input_data)

    # Get matrix sizes
    n, m = matrix.shape

    # print()
    # print("N: %d, M: %d" % (n, m))
    # print("Matrix:\n%s" % str(matrix))

    for j in range(int(m / 2)):
        for i in range(j, n - j - 1):
            # print("m / 2: %d, n - j - 1: %d" % (int(m / 2), n - j - 1))
            # print()
            # print()
            # print("#################\nPass %d:" % (j * (m / 2) + i))
            # print("i: %d, j: %d, value: %d" % (i, j, matrix[i, j]))
            # print()

            top = j
            left= j
            bottom = n - j - 1
            right = m - j - 1


            p1 = i, j
            p2 = bottom, i
            p3 = bottom - i, right
            p4 = j, right - i

            matrix_swap(matrix, p1, p2)
            matrix_swap(matrix, p2, p3)
            matrix_swap(matrix, p3, p4)

            # print()
            # print("Matrix:\n%s" % str(matrix))

    # print()
    # print()
    return matrix.tolist()


# Can also skip classes
#@unittest.skip("fizzbuzz")
class TestMatrixRotation(unittest.TestCase):

    def assert_matrix_rotation(self, input_data, expected):
        actual = matrix_rotation(input_data)
        self.assertEqual(actual, expected)


    #@unittest.skip("fizzbuzz")
    def test1(self):
        input_data = [
            [1,0],
            [0,1]
        ]

        expected = [
            [0,1],
            [1,0]
        ]

        self.assert_matrix_rotation(input_data, expected)


    #@unittest.skip("fizzbuzz")
    def test2(self):
        input_data = [
            [1,2],
            [1,2]
        ]

        expected = [
            [1,1],
            [2,2]
        ]

        self.assert_matrix_rotation(input_data, expected)


    #@unittest.skip("fizzbuzz")
    def test3(self):
        input_data = [
            [1,2,3],
            [4,5,6],
            [7,8,9]
        ]

        expected = [
            [7,4,1],
            [8,5,2],
            [9,6,3]
        ]

        self.assert_matrix_rotation(input_data, expected)


    #@unittest.skip("fizzbuzz")
    def test4(self):
        input_data = [
            [1,2,3,4],
            [1,2,3,4],
            [1,2,3,4],
            [1,2,3,4],
        ]

        expected = [
            [1,1,1,1],
            [2,2,2,2],
            [3,3,3,3],
            [4,4,4,4],
        ]
        
        self.assert_matrix_rotation(input_data, expected)

    def test5(self):
        input_data = [
            [1,2,3,4,5],
            [1,2,3,4,5],
            [1,2,3,4,5],
            [1,2,3,4,5],
            [1,2,3,4,5],
        ]

        expected = [
            [1,1,1,1,1],
            [2,2,2,2,2],
            [3,3,3,3,3],
            [4,4,4,4,4],
            [5,5,5,5,5],
        ]

        self.assert_matrix_rotation(input_data, expected)


    def test6(self):
        input_data = [
            [1,2,3,4,5,6],
            [1,2,3,4,5,6],
            [1,2,3,4,5,6],
            [1,2,3,4,5,6],
            [1,2,3,4,5,6],
            [1,2,3,4,5,6],
        ]

        expected = [
            [1,1,1,1,1,1],
            [2,2,2,2,2,2],
            [3,3,3,3,3,3],
            [4,4,4,4,4,4],
            [5,5,5,5,5,5],
            [6,6,6,6,6,6],
        ]

        self.assert_matrix_rotation(input_data, expected)


if __name__ == '__main__':
    unittest.main()

