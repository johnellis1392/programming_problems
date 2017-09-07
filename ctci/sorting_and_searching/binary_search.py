"""
Binary search
"""

__all__ = ['binary_search']
__version__ = '0.1'
__author__ = 'John Ellis'


import unittest
# import ipdb


def __binary_search__(array, lower, upper, value):
    # print(array, lower, upper, value)
    middle = int((lower + upper) / 2)
    if array[middle] == value:
        return middle
    elif upper <= lower:
        return None

    # if upper <= lower:
    #     if array[lower] == value:
    #         return lower
    #     return None

    if array[middle] < value:
        return __binary_search__(array, middle + 1, upper, value)
    return __binary_search__(array, lower, middle - 1, value)


def binary_search(array, value):
    """
    Perform a binary seatch on an array of values
    that's shifted by some number of elements.
    """

    return __binary_search__(array, 0, len(array) - 1, value)


class TestModifiedBinarySearch(unittest.TestCase):
    """
    Unit tests for modified binary search function
    """

    def test1(self):
        """
        Basic unit test
        """

        input_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        search_value = 6
        expected = 5
        actual = binary_search(input_data, search_value)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()
