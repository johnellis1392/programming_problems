"""
Python3 Implementation of an unbalanced BinaryTree
"""

__all__ = ['Node', 'BinaryTree']
__version__ = '1.0'
__author__ = 'John Ellis'

import collections
import random
import unittest

# import ipdb
# ipdb.set_trace(context=DEBUG_CONTEXT)

DEBUG_CONTEXT = 25


def random_array(range_bounds=(10, 20), value_bounds=(0, 1e6)):
    """
    Generate a random array of input data for testing
    """

    i, j = range_bounds
    lower_bound, upper_bound = value_bounds
    length = random.randint(i, j)
    values = [random.randint(lower_bound, upper_bound) for _ in range(length)]
    return values


# Utility function for generating random test data
def random_tree(range_bounds=(10, 20), value_bounds=(0, 1e6)):
    """
    Generate a random tree of data for testing
    """
    tree = BinaryTree()
    i, j = range_bounds
    lower_bound, upper_bound = value_bounds
    values = random_array(
        range_bounds=(i, j),
        value_bounds=(lower_bound, upper_bound)
    )
    length = len(values)

    for k in values:
        tree.insert(k)
    return tree, length, values


class Node():
    """
    Tree node for a Binary BinaryTree
    """

    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def height(self):
        """
        Get the height of the current node.
        """
        left = self.left.height() if self.left else -1
        right = self.right.height() if self.right else -1
        return 1 + max(left, right)

    def isleaf(self):
        """
        Determine whether or not the current node is a leaf
        """
        return not self.left and not self.right

    def __repr__(self):
        value = repr(self.value)
        left = repr(self.left)
        right = repr(self.right)
        return 'Node(value=%s, left=%s, right=%s)' % (value, left, right)


class BinaryTree():
    """
    Unbalanced Binary BinaryTree class
    """

    def __init__(self):
        self.root = None

    def __insert__(self, value, node):
        if not node:
            return Node(value)
        elif value < node.value:
            node.left = self.__insert__(value, node.left)
        else:
            node.right = self.__insert__(value, node.right)
        return node

    def insert(self, value):
        """
        Insert an element into the Tree
        """

        self.root = self.__insert__(value, self.root)
        return self

    def __remove__(self, value, node):
        pass

    def remove(self, value):
        """
        Remove an element from the Tree
        """

        self.root = self.__remove__(value, self.root)
        return self

    def __len__(self):
        if not self.root:
            return 0

        stack = collections.deque()
        stack.append(self.root)
        length = 0

        while stack:
            node = stack.pop()
            length += 1
            if node.left:
                stack.append(node.left)
            if node.right:
                stack.append(node.right)

        return length

    def __repr__(self):
        return repr(self.root)


class TestTree(unittest.TestCase):
    """
    Unit Tests for BinaryTree class
    """

    def __check__(self, node, values):
        if not node:
            return True
        left = self.__check__(node.left, values)
        middle = node.value == values.popleft()
        right = self.__check__(node.right, values)
        return left and middle and right

    def check(self, tree, values):
        """
        Do an in-order traversal of the tree, verifying all the values
        in the `values` array are contained in order in the tree.
        """

        node = tree.root
        sorted_values = collections.deque(sorted(values))
        result = self.__check__(node, sorted_values)
        if sorted_values:
            # Values still left in array; validation failed
            return False
        return result

    def __run_test__(self, values):
        tree = BinaryTree()
        for i in values:
            tree.insert(i)
        self.assertEqual(len(tree), len(values))
        self.assertTrue(self.check(tree, values))
        return tree

    def test_insert1(self):
        """
        Test inserting basic data
        """

        values = [1, 2, 3]
        tree = self.__run_test__(values)
        self.assertEqual(tree.root.value, 1)
        self.assertEqual(tree.root.right.value, 2)
        self.assertEqual(tree.root.right.right.value, 3)

    def test_insert2(self):
        """
        Test inserting elements in a sorted order
        """

        values = [2, 1, 3]
        tree = self.__run_test__(values)
        self.assertEqual(tree.root.value, 2)
        self.assertEqual(tree.root.right.value, 3)
        self.assertEqual(tree.root.left.value, 1)

    @unittest.skip('Remove not implemented')
    def test_remove1(self):
        """
        Test removing data from basic Binary Tree
        """
        pass


if __name__ == '__main__':
    unittest.main()
