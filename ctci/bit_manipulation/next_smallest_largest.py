"""
Implementation of CTCI problem 5.3
"""

__all__ = ['next_smallest_largest']
__version__ = '0.1'
__author__ = 'John Ellis'


import unittest


def next_smallest_largest(value):
    """
    Calculate the next smallest and next largest numbers relative to
    the given value, that have the same number of 1's as the given value.
    """

    smaller = None
    larger = None

    for i in range(0, 64 - 1):
        if value & (2 ** i) != 0 and value & (2 ** (i + 1)) == 0:
            larger = value ^ (2 ** i) | (2 ** (i + 1))
            break

    for j in range(1, 64):
        if value & (2 ** j) != 0 and value & (2 ** (j - 1)) == 0:
            smaller = value ^ (2 ** j) | (2 ** (j - 1))
            break

    return smaller, larger


class TestNextSmallestLargest(unittest.TestCase):
    """
    Unit tests for next_smallest_largest
    """

    def test1(self):
        """
        Unit test for next_smallest_largest
        Case:
        - value:    110
        - smaller:  101
        - larger:   1010
        """

        input_data = 0b110
        smaller, larger = next_smallest_largest(input_data)
        self.assertEqual(smaller, 0b101)
        self.assertEqual(larger, 0b1010)

    def test2(self):
        """
        Unit test for next_smallest_largest
        - value:    1101
        - smaller:  1011
        - larger:   1110
        """

        input_data = 0b1101
        smaller, larger = next_smallest_largest(input_data)
        self.assertEqual(smaller, 0b1011)
        self.assertEqual(larger, 0b1110)

    def test3(self):
        """
        Unit test for next_smallest_largest
        - value:    10101
        - smaller:  10011
        - larger:   10110
        """

        input_data = 0b10101
        smaller, larger = next_smallest_largest(input_data)
        self.assertEqual(smaller, 0b10011)
        self.assertEqual(larger, 0b10110)


if __name__ == '__main__':
    unittest.main()
