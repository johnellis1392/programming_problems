"""
Implementation of CTCI problem 5.1
"""

__all__ = ['bit_range_assign']
__version__ = '0.1'
__author__ = 'John Ellis'

import unittest


def bit_range_assign(n_value, m_value, i, j):
    """
    Assign the bit value of m to the position in n
    starting at i and going up to j.
    """

    mask = (2 ** 64 - 1) ^ (2 ** (j + 1) - 1) ^ (2 ** i - 1)
    m_prime = m_value << i
    return n_value & mask | m_prime


class TestBitRangeAssign(unittest.TestCase):
    """
    Unit tests for bit_range_assign
    """

    def test1(self):
        """
        Test Case:
        - n:        100 0000 0000
        - m:        1 0101
        - i, j:     (2, 6)
        - output:   100 0101 0100
        """

        input_data = 0b10000000000, 0b10101, 2, 6
        expected = 0b10001010100
        actual = bit_range_assign(*input_data)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()
