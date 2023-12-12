"""
Implementation for the max stock profit problem.
"""

__all__ = ['max_profits']
__version__ = '0.1'
__author__ = 'John Ellis'
import unittest



def max_profits(array):
    """
    Max stock profits function.
    """

    result = []
    i = 0
    while i < len(array):
        j = i + 1
        while j < len(array) and array[j] - array[j - 1] >= 0:
            j += 1

        if array[j - 1] - array[i] >= 0:
            result.append((i, j - 1))
            i = j
        else:
            i += 1

    return result


class TestMaxProfits(unittest.TestCase):
    """
    Unit tests for max profits
    """

    def test1(self):
        """
        Default test case.
        """

        input_data = [100, 180, 260, 310, 40, 535, 695]
        expected = [(0, 3), (4, 6)]
        actual = max_profits(input_data)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()
