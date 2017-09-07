"""
Implementation for a modified binary search function
for a shifted array
"""

__all__ = ['modified_bsearch']
__version__ = '0.1'
__author__ = 'John Ellis'


import unittest
# import ipdb


def __modified_bsearch__(array, lower, upper, value):
    # ipdb.set_trace(context=25)
    middle = int((lower + upper) / 2)
    if array[middle] == value:
        return middle
    elif upper <= lower:
        return None

    # if upper < lower:
    #     if array[lower] == value:
    #         return lower
    #     return None

    if array[lower] <= value and value <= array[upper]:
        if array[middle] < value:
            return __modified_bsearch__(array, middle + 1, upper, value)
        return __modified_bsearch__(array, lower, middle - 1, value)
    else:
        if value < array[upper]:
            return __modified_bsearch__(array, middle + 1, upper, value)
        return __modified_bsearch__(array, lower, middle - 1, value)


def modified_bsearch(array, value):
    """
    Perform a binary seatch on an array of values
    that's shifted by some number of elements.
    """

    return __modified_bsearch__(array, 0, len(array) - 1, value)


class TestModifiedBinarySearch(unittest.TestCase):
    """
    Unit tests for modified binary search function
    """

    def test1(self):
        """
        Basic unit test
        """

        input_data = [9, 10, 1, 2, 3, 4, 5, 6, 7, 8]
        search_value = 6
        expected = 7
        actual = modified_bsearch(input_data, search_value)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()
