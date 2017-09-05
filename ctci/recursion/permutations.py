"""
Implementation of CTCI problem 8.4
"""

__all__ = ['permutations']
__version__ = '0.1'
__author__ = 'John Ellis'

import unittest

import ipdb


def permutations(string):
    """
    Calculate all possible permutations for the given input string.
    """

    # If string is empty or only one character long, it only has one combination:
    # ipdb.set_trace(context=25)
    if not string:
        return []
    elif len(string) == 1:
        return [string]

    char = string[0]
    substring = string[1:]
    substring_length = len(substring)
    child_combinations = permutations(substring)

    result = []
    for i in range(0, substring_length + 1):
        for combination in child_combinations:
            new_combination = combination[:i] + char + combination[i:]
            result.append(new_combination)

    return result


class TestPermutations(unittest.TestCase):
    """
    Unit Tests for permutations function
    """

    def test1(self):
        """
        Test a simple permutation case:
        - input: 'abc'
        - output: ['abc', 'acb', 'bac', 'bca', 'cab', 'cba']
        """

        input_data = 'abc'
        expected = ['abc', 'acb', 'bac', 'bca', 'cab', 'cba']
        actual = permutations(input_data)
        self.assertEqual(sorted(expected), sorted(actual))


if __name__ == '__main__':
    unittest.main()
