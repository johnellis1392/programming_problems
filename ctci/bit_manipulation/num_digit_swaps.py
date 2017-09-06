"""
Implementation of CTCI problem 5.5
"""

__all__ = ['num_digit_swaps']
__version__ = '0.1'
__author__ = 'John Ellis'


import unittest


def num_digit_swaps(num1, num2):
    """
    Determine the number of digits to swap in n
    to convert it into m.
    """

    differing_digits = num1 ^ num2
    result = 0
    for i in range(64):
        result += (differing_digits >> i) & 1
    return result


class TestNumDigitSwaps(unittest.TestCase):
    """
    Unit tests for num_digit_swaps
    """

    def test1(self):
        """
        Test Case 1:
        - n:        31, (0b11111)
        - m:        14, (0b1110)
        - output:   2
        """

        input_data = 31, 14
        expected = 2
        actual = num_digit_swaps(*input_data)
        self.assertEqual(expected, actual)

    def test2(self):
        """
        Test Case 1:
        - n:        32, (0b100000)
        - m:        32, (0b100000)
        - output:   0
        """

        input_data = 32, 32
        expected = 0
        actual = num_digit_swaps(*input_data)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()
