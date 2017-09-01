"""
tree_is_balanced implementation:

Implement a function to check if a tree is balanced.
For the purposes of this question, a balanced tree is
defined to be a tree such that no two leaf nodes differ
in distance from the root by more than one.
"""

__all__ = ['tree_is_balanced']
__version__ = '0.1'
__author__ = 'John Ellis'

import random
import unittest

import ipdb

from avl_tree import AVLTree
from binary_tree import BinaryTree

DEBUG_CONTEXT = 25


def __height__(node):
    if not node:
        return -1
    left = __height__(node.left)
    right = __height__(node.right)
    return 1 + max(left, right)


def __tree_is_balanced__(node):
    height_left = __height__(node.left)
    height_right = __height__(node.right)

    if abs(height_left - height_right) > 1:
        # Heights are Unbalanced
        return False

    # Heights are Balanced
    return True


def tree_is_balanced(tree):
    """
    Validate that a given binary tree is balanced
    """
    return __tree_is_balanced__(tree.root)


# ################# #
# Testing Utilities #
# ################# #


DEFAULT_RANGE_BOUNDS = (10, 50)
DEFAULT_VALUE_BOUNDS = (0, 1e6)


def gen_random_array(range_bounds=DEFAULT_RANGE_BOUNDS, value_bounds=DEFAULT_VALUE_BOUNDS):
    """
    Generate a random array of integers for unit testing
    """
    i, j = range_bounds
    lower_bound, upper_bound = value_bounds
    length = random.randint(i, j)
    values = [random.randint(lower_bound, upper_bound) for _ in range(length)]
    return values


def fill_tree(tree, values):
    """
    Fill tree with the given array of values
    """
    for i in values:
        tree.insert(i)
    return tree, values


def fill_random_tree(tree, range_bounds, value_bounds):
    """
    Fill the given tree structure with randomly-generated array data
    """
    values = gen_random_array(range_bounds, value_bounds)
    return fill_tree(tree, values)


def gen_balanced_tree(range_bounds=DEFAULT_RANGE_BOUNDS, value_bounds=DEFAULT_VALUE_BOUNDS):
    """
    Generate a balanced AVLTree from random data
    """
    tree = AVLTree()
    return fill_random_tree(tree, range_bounds, value_bounds)


def gen_unbalanced_tree(range_bounds=DEFAULT_RANGE_BOUNDS, value_bounds=DEFAULT_VALUE_BOUNDS):
    """
    Generate an unbalanced BinaryTree from random data
    """
    original_tree = BinaryTree()
    tree, values = fill_random_tree(original_tree, range_bounds, value_bounds)

    # Tree may be balanced; insert extra data
    max_value = max(values) + 1
    debalancing_data = list(range(max_value, max_value + 10))
    tree, _ = fill_tree(tree, debalancing_data)
    values.extend(debalancing_data)
    return tree, values


class TestTreeIsBalanced(unittest.TestCase):
    """
    Unit Tests for the tree_is_balanced function
    """

    def test_unblanced_tree1(self):
        """
        Test against an unbalanced binary tree
        """
        values = [1, 2, 3]
        tree, _ = fill_tree(BinaryTree(), values)
        actual = tree_is_balanced(tree)
        self.assertFalse(actual)

    def test_unbalanced_tree2(self):
        """
        Test against generated ranges of numbers
        """
        length = 30
        for i in range(3, length + 1):
            values = list(range(i))
            tree, _ = fill_tree(BinaryTree(), values)
            actual = tree_is_balanced(tree)
            self.assertFalse(actual)

    def test_unbalanced_tree3(self):
        """
        Test against randomly-generated unbalanced tree data
        """
        num_tests = 50
        print()
        print()
        for _ in range(num_tests):
            tree, values = gen_unbalanced_tree()
            print('Testing for values: %s' % repr(values))
            actual = tree_is_balanced(tree)
            self.assertFalse(actual)

    def test_balanced_tree1(self):
        """
        Test against a balanced binary tree
        """
        values = [1, 2, 3]
        tree, _ = fill_tree(AVLTree(), values)
        actual = tree_is_balanced(tree)
        self.assertTrue(actual)

    def test_balanced_tree2(self):
        """
        Test against generated ranges of numbers
        """
        length = 30
        for i in range(3, length + 1):
            values = list(range(i))
            tree, _ = fill_tree(AVLTree(), values)
            actual = tree_is_balanced(tree)
            self.assertTrue(actual)

    def test_balanced_tree3(self):
        """
        Test against randomly-generated balanced tree data
        """
        num_tests = 50
        for _ in range(num_tests):
            tree, _ = gen_balanced_tree()
            actual = tree_is_balanced(tree)
            self.assertTrue(actual)


if __name__ == '__main__':
    unittest.main()
