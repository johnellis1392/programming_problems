import unittest
import numpy as np


def zeroify(l):
    matrix = np.array(l)
    n, m = matrix.shape

    points = []
    for i in range(n):
        for j in range(m):
            if matrix[i, j] == 0:
                points.append((i, j))

    for p in points:
        x, y = p
        for i in range(n): matrix[i, y] = 0
        for j in range(m): matrix[x, j] = 0

    return matrix.tolist()


class TestZeroify(unittest.TestCase):

    def test1(self):
        input_data = [
            [1,2,3],
            [4,0,6],
            [7,8,9],
        ]

        expected = [
            [1,0,3],
            [0,0,0],
            [7,0,9],
        ]

        self.assertEqual(expected, zeroify(input_data))

    def test2(self):
        input_data = [
            [1,1,1,0],
            [2,2,2,2],
            [3,3,3,3],
            [0,4,4,4],
        ]

        expected = [
            [0,0,0,0],
            [0,2,2,0],
            [0,3,3,0],
            [0,0,0,0],
        ]

        self.assertEqual(expected, zeroify(input_data))



if __name__ == '__main__':
    unittest.main()
