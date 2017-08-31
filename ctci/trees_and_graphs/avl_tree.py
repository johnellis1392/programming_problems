"""
AVL Tree Implementation in Python3
"""

__all__ = ['Node', 'AVLTree']
__version__ = '0.1'
__author__ = 'John Ellis'


import math
import random
import unittest
from collections import deque
from itertools import permutations

# from ipdb import set_trace as debug

DEBUG_CONTEXT = 20


def random_array(range_bounds=(10, 20), value_bounds=(0, 1e6)):
    """
    Generate a random array of input data
    """

    i, j = range_bounds
    lower_bound, upper_bound = value_bounds

    length = random.randint(i, j)
    values = [random.randint(lower_bound, upper_bound) for _ in range(length)]

    return values


def random_balanced_tree(range_bounds=(10, 20), value_bounds=(0, 1e6)):
    """
    Generate a balanced tree from random data
    """
    tree = AVLTree()
    i, j = range_bounds
    lower_bound, upper_bound = value_bounds

    values = random_array((i, j), (lower_bound, upper_bound))
    length = len(values)
    for k in values:
        tree.insert(k)

    return tree, length, values


class Node():
    """
    AVL Tree Node
    """

    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

    def __eq__(self, other):
        # return type(other) == Node and other.value == self.value
        return isinstance(other, Node) and other.value == self.value

    def __repr__(self):
        left = id(self.left) if self.left else None
        right = id(self.right) if self.right else None
        return 'Node(value=%d, left=%s, right=%s)' % (self.value, str(left), str(right))

    def height(self):
        """
        Get the height of the current node.
        Returns 0 if the node has no children.
        """
        height_left = self.left.height() if self.left else -1
        height_right = self.right.height() if self.right else -1
        return 1 + max([height_left, height_right])

    def balancefactor(self):
        """
        Calculate the balance factor of the current node.
        The balance factor is defined as some integer indicating
        how balanced the current node is in terms of its children.

        A value of `balancefactor in [-1, 0, 1]` is considered valid,
        a value of `balancefactor < -1` is considered left-heavy,
        and a value of `balancefactor > 1` is considered right-heavy.
        """
        height_left = self.left.height() if self.left else -1
        height_right = self.right.height() if self.right else -1
        return height_right - height_left


class AVLTree():
    """
    AVL Tree Implementation; Balanced Binary Tree
    """

    def __init__(self):
        self.root = None

    def __height__(self, node):
        if node is None:
            return -1
        return 1 + max(self.__height__(node.left), self.__height__(node.right))

    @staticmethod
    def __rotateright__(node):
        node1 = node
        node2 = node.left
        temp1 = node.left.right

        node2.right = node1
        node1.left = temp1
        node = node2

        return node

    @staticmethod
    def __rotateleft__(node):
        node1 = node
        node2 = node.right
        temp1 = node.right.left

        node2.left = node1
        node1.right = temp1
        node = node2

        return node

    def __balancefactor__(self, node):
        if node is None:
            return 0
        return self.__height__(node.right) - self.__height__(node.left)

    def __balance__(self, node):
        if node is None:
            return node

        balancefactor = self.__balancefactor__(node)
        if balancefactor < 0:
            # Left heavy
            if self.__balancefactor__(node.left) > 0:
                node.left = self.__rotateleft__(node.left)
            node = self.__rotateright__(node)

        elif balancefactor > 0:
            # Right heavy
            if self.__balancefactor__(node.right) < 0:
                node.right = self.__rotateright__(node.right)
            node = self.__rotateleft__(node)

        return node

    def height(self):
        """
        Calculate the height of the tree.
        """
        return self.__height__(self.root)

    def __insert__(self, value, node):
        if not node:
            node = Node(value)
        elif value < node.value:
            node.left = self.__insert__(value, node.left)
        else:
            node.right = self.__insert__(value, node.right)
        return self.__balance__(node)

    def insert(self, value):
        """
        Insert some element 'value' into the tree.
        """
        self.root = self.__insert__(value, self.root)
        return self

    def __find__(self, value, node):
        if node is None:
            return None
        elif value == node.value:
            return node
        elif value < node.value:
            return self.__find__(value, node.left)

        return self.__find__(value, node.right)

    def find(self, value):
        """
        Find the node corresponding to 'value' in the tree.
        """
        return self.__find__(value, self.root)

    # TODO: Implement this method
    def __remove__(self, value, node):
        pass

    def remove(self, value):
        """
        Remove the specified element from the tree.
        """
        self.root = self.__remove__(value, self.root)
        return self

    def __len__(self):
        if not self.root:
            return 0

        stack = deque()
        stack.append(self.root)
        length = 0

        # NOTE: Empty sequences evaluate to False in python;
        # this while loop exits when the sequence is expended.
        while stack:
            node = stack.pop()
            length += 1
            if node.left:
                stack.append(node.left)
            if node.right:
                stack.append(node.right)
        return length

    def __debug_string__(self, node, tabspace):
        string = ['  %s* %d' % (' ' * (tabspace * 2), node.value)]
        if node.left:
            string += self.__debug_string__(node.left, tabspace + 1)
        if node.right:
            string += self.__debug_string__(node.right, tabspace + 1)
        return string

    def debug_string(self):
        """
        Generate a debugging string for the tree's current state.
        """
        if not self.root:
            return '  * None'
        return '\n'.join(self.__debug_string__(self.root, 0))

    def debug_print(self):
        """
        Print a debugging string for the tree's current state.
        """
        debug_string = self.debug_string()
        print()
        print('  # Tree')
        print(debug_string)
        print()


class TestBalancedTree(unittest.TestCase):
    """
    Unit Tests for AVLTree class
    """

    def __check__(self, node, values):
        if not node:
            return True
        left_valid = self.__check__(node.left, values)
        node_valid = node.value == values.popleft()
        right_valid = self.__check__(node.right, values)
        return left_valid and node_valid and right_valid

    def check(self, tree, values):
        """
        Validate all values in the given tree against the given
        input array. This function does an in-order traversal of
        the tree, and checks every element against all the sorted
        values in the values array.
        """

        queue = deque(sorted(values))
        result = self.__check__(tree.root, queue)
        if queue:
            # Not all values checked; tree is of invalid length
            return False
        return result

    def run_test(self, input_data):
        """
        Given some input_data array, create an AVLTree from
        the list and validate its expected height, length,
        and order.
        """
        # print('Running test case for: %s' % str(input_data))

        tree = AVLTree()
        length = len(input_data)
        for i in input_data:
            tree.insert(i)

        height = tree.height()
        logh = int(math.log(length, 2))
        self.assertEqual(len(tree), length)
        self.assertTrue(logh <= height <= logh + 1)
        self.assertTrue(self.check(tree, input_data))

    def test_insert1(self):
        """
        Validate a correctly-balanced tree for integers
        1 - 7
        """

        length = 7
        values = list(range(1, length + 1))
        self.run_test(values)

    def test_insert2(self):
        """
        Validate the proper structure of the AVLTree over the
        whole insert process
        """

        tree = AVLTree()
        tree.insert(1)
        tree.insert(2)
        tree.insert(3)

        self.assertEqual(len(tree), 3)
        self.assertEqual(tree.height(), 1)
        self.assertEqual(tree.root.value, 2)
        self.assertEqual(tree.root.left.value, 1)
        self.assertEqual(tree.root.right.value, 3)
        self.assertTrue(self.check(tree, [1, 2, 3]))

    def test_insert3(self):
        """
        Test AVLTree against some random input data
        """

        num_random_tests = 100
        for _ in range(num_random_tests):
            values = random_array(range_bounds=(10, 50))
            self.run_test(values)

    def test_insert4(self):
        """
        Test AVLTree for the set of all lists of integers
        in the range: [1], [1, 2], ... [1, 2, ..., 30]
        """

        for i in range(1, 30 + 1):
            values = list(range(1, i + 1))
            self.run_test(values)

    def test_insert5(self):
        """
        Test AVLTree for a specific failing case (found from random testing data)
        """

        values = [849403, 818924, 865206, 69780, 154144, 464240, 642839,
                  439150, 966005, 519811, 637830, 728303, 880476, 77348, 524712]
        self.run_test(values)

    def test_insert6(self):
        """
        Test AVLTree against simplified version of failing case from
        test_insert5 (the insert order specifically failed for certain
        implementations).
        """

        values = [11, 10, 12, 0, 2, 4, 8, 3, 14, 5, 7, 9, 13, 1, 6]
        self.run_test(values)

    def test_insert7(self):
        """
        Test AVLTree for list of 14 elements
        """

        length = 14
        values = list(range(1, length + 1))
        self.run_test(values)

    def test_insert8(self):
        """
        Test AVLTree for all permutations of the set of numbers:
        [1, 2, ... (2 ** 3 - 1)]
        ie: Test all possible input sets for a depth=4 tree.

        NOTE: setting depth=4 makes the tests run too slow.
        """

        depth = 3
        input_set = list(range(1, 2 ** depth - 1))
        for input_data in permutations(input_set):
            self.run_test(input_data)

    @unittest.skip('Debugging')
    def test_remove1(self):
        """
        Test Removing an element from a simple AVLTree
        """

        tree = AVLTree()
        values = [1, 2, 3]
        for i in values:
            tree.insert(i)

        for j in values:
            expected = Node(j)
            actual = tree.find(j)
            self.assertEqual(expected, actual)

    @unittest.skip('Debugging')
    def test_find1(self):
        """
        Test finding an element in a simple AVLTree
        """

        tree = AVLTree()
        values = deque([1, 2, 3])
        for i in values:
            tree.insert(i)
        self.assertEqual(len(tree), 3)

        # Remove all elements
        while values:
            to_remove = values.pop()
            tree.remove(to_remove)
            self.assertEqual(len(tree), len(values))
            self.assertTrue(self.check(tree, list(values)))

        # Validate all elements are removed from tree
        self.assertEqual(len(tree), 0)


if __name__ == '__main__':
    unittest.main()
