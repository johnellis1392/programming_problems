import math
import random
import unittest
from collections import deque

from ipdb import set_trace as debug

DEBUG_CONTEXT = 20


def random_balanced_tree(range_bounds=(10, 20), value_bounds=(0, 1e6)):
    t = BalancedTree()
    i, j = range_bounds
    x, y = value_bounds

    n = random.randint(i, j)
    values = [random.randint(x, y) for _ in range(n)]
    for k in values:
        t.insert(k)

    return t, n, values


class Node():

    def __init__(self, v):
        self.value = v
        self.left = None
        self.right = None

    def __eq__(self, other):
        return type(other) == Node and other.value == self.value


class BalancedTree():

    def __init__(self):
        self.root = None

    def __height__(self, n):
        if not n:
            return -1
        else:
            return 1 + max(self.__height__(n.left), self.__height__(n.right))

    def __balance__(self, n):
        if not n:
            return None

        h_l = self.__height__(n.left)
        h_r = self.__height__(n.right)
        # debug(context = DEBUG_CONTEXT)
        if abs(h_l - h_r) > 1:
            if h_l > h_r:
                left = n.left
                temp = left.right
                left.right = n
                n.left = temp
                n = left

            if h_l <= h_r:
                right = n.right
                temp = right.left
                right.left = n
                n.right = temp
                n = right

        return n

    # TODO: Finish this modified algorithm
    # def __leftmost__(self, n):
    #     if n == None:
    #         return None
    #     elif n.left == None:
    #         return n
    #     else:
    #         return self.__leftmost__(n.left)
    #
    # def __rightmost__(self, n):
    #     if n == None:
    #         return None
    #     elif n.right == None:
    #         return n
    #     else:
    #         return self.__rightmost__(n.right)
    #
    # def __balance__(self, n):
    #     if not n:
    #         return None
    #
    #     hl = self.__height__(n.left)
    #     hr = self.__height__(n.right)
    #     if abs(hl - hr) > 1:
    #         if hl > hr:
    #             # Shift clockwise
    #             n.left = temp = self.__rightmost__(n.left)
    #             left = n.left
    #
    #         else:
    #             # Shift counter-clockwise
    #             pass
    #
    #     return n

    def height(self):
        return self.__height__(self.root)

    def __insert__(self, v, n):
        if not n:
            n = Node(v)
        elif v < n.value:
            n.left = self.__insert__(v, n.left)
        else:
            n.right = self.__insert__(v, n.right)
        # debug(context = DEBUG_CONTEXT)
        return self.__balance__(n)

    def insert(self, v):
        self.root = self.__insert__(v, self.root)
        return self

    # TODO: Come back to this
    # def __remove__(self, v, n):
    #     if not n:
    #         return None
    #     elif v == n.value:
    #         # If both leaves are empty, just return null
    #         # If left is empty, return leftmost element of right tree
    #         # If right is empty, return rightmost element of left tree
    #         # Finally, balance
    #         if not n.left and not n.right:
    #             return None
    #         elif not n.left:
    #             n_p = n.right
    #             if n_p.left != None:
    #                 while n_p.left.left != None:
    #                     n_p = n_p.left
    #                 temp = n_p.left
    #                 n_p.left = None
    #                 n_p = temp
    #             else:
    #
    #             return self.__balance__(n_p)
    #         else:
    #             n_p = n
    #             return self.__balance__(n_p)
    #     elif v < n.value:
    #         return self.__balance__(self.__remove__(v, n.left))
    #     else:
    #         return self.__balance__(self.__remove__(v, n.right))

    # def remove(self, v):
    #     self.root = self.__remove__(v, self.root)
    #     return self

    def __find__(self, v, n):
        if not n:
            return None
        elif v == n.value:
            return n
        elif v < n.value:
            return self.__find__(v, n.left)
        else:
            return self.__find__(v, n.right)

    def find(self, v):
        return self.__find__(v, self.root)

    def __len__(self):
        if not self.root:
            return 0
        stack = deque()
        stack.append(self.root)
        n = 0
        while len(stack) != 0:
            node = stack.pop()
            n += 1
            if node.left:
                stack.append(node.left)
            if node.right:
                stack.append(node.right)
        return n

    def __debug_string__(self, n, tabspace):
        # print('  %s* %d' % (' ' * (tabspace * 2), n.value))
        s = ['  %s* %d' % (' ' * (tabspace * 2), n.value)]
        if n.left:
            s += self.__debug_string__(n.left, tabspace + 1)
        if n.right:
            s += self.__debug_string__(n.right, tabspace + 1)
        return s

    def debug_string(self):
        if not self.root:
            return '  * None'
        return '\n'.join(self.__debug_string__(self.root, 0))

    def debug_print(self):
        s = self.debug_string()
        print()
        print('  # Tree')
        print(s)
        print()


class TestBalancedTree(unittest.TestCase):

    def __check__(self, node, values):
        if not node:
            return True
        rl = self.__check__(node.left, values)
        rn = node.value == values.popleft()
        rr = self.__check__(node.right, values)
        return rl and rn and rr

    def check(self, tree, values):
        v = deque(values)
        result = self.__check__(tree.root, v)
        if len(v) != 0:
            # Not all values checked; tree is of invalid length
            return False
        else:
            return True

    @unittest.skip('Debugging')
    def test_insert1(self):
        t = BalancedTree()
        n = 7
        v = []
        for i in range(1, n + 1):
            v.append(i)
            t.insert(i)

        self.assertEqual(len(t), n)
        self.assertEqual(t.height(), int(math.log(n, 2)))
        self.assertTrue(self.check(t, v))

    @unittest.skip('Debugging')
    def test_insert2(self):
        t = BalancedTree()
        t.insert(1)
        t.insert(2)
        t.insert(3)

        self.assertEqual(len(t), 3)
        self.assertEqual(t.height(), 1)
        self.assertEqual(t.root.value, 2)
        self.assertEqual(t.root.left.value, 1)
        self.assertEqual(t.root.right.value, 3)
        self.assertTrue(self.check(t, [1, 2, 3]))

    @unittest.skip('Debugging')
    def test_insert3(self):
        for _ in range(50):
            t, n, v = random_balanced_tree()
            self.assertEqual(len(t), n)

            h = t.height()
            logh = math.floor(math.log(n, 2))
            # print(' ** n = %d, h = %d, log(h, 2) = %f' % (n, h, logh))
            debug_string = """

 * Test Failed:
 ** input: "%s"
 ** n = %d, h = %d, log(h, 2) = %f

 * Tree:\n%s
""" % (str(v), n, h, math.log(h, 2), t.debug_string())
            # self.assertTrue(logh <= h <= logh + 1, msg = '\n\n * Test failed:\n ** input: "%s":\n ** n = %d, h = %d, log(h, 2) = %f' % (str(v), n, h, math.log(h, 2)))

            self.assertTrue(logh <= h <= logh + 1, msg=debug_string)
            self.assertTrue(self.check(t, v))

    @unittest.skip('Debugging')
    def test_insert4(self):
        for i in range(1, 30 + 1):
            t = BalancedTree()
            v = list(range(1, i + 1))
            for j in v:
                t.insert(j)

            # t.debug_print()
            self.assertEqual(len(t), i)
            self.assertEqual(t.height(), int(math.log(i, 2)))
            self.assertTrue(self.check(t, v))

    @unittest.skip('Debugging')
    def test_insert5(self):
        t = BalancedTree()
        values = [849403, 818924, 865206, 69780, 154144, 464240, 642839,
                  439150, 966005, 519811, 637830, 728303, 880476, 77348, 524712]
        n = len(values)
        for i in values:
            # debug(context = DEBUG_CONTEXT)
            t.insert(i)
            # t.debug_print()

        h = t.height()
        logh = int(math.log(h, 2))
        self.assertEqual(len(t), n)
        debug_string = """

* Values: %s
** n = %d
** h = %d
** log(h, 2) = %f

* Tree:
%s
""" % (str(values), n, h, math.log(h, 2), t.debug_string())

        self.assertTrue(logh <= h <= logh + 1, msg=debug_string)
        self.assertTrue(self.check(t, values))

    # @unittest.skip('Debugging')
    def test_insert6(self):
        t = BalancedTree()
        values = [11, 10, 12, 0, 2, 4, 8, 3, 14, 5, 7, 9, 13, 1, 6]
        n = len(values)
        for i in values:
            t.insert(i)
        h = t.height()
        logh = int(math.log(h, 2))
        self.assertEqual(len(t), n)
        self.assertTrue(logh <= h <= logh + 1)
        self.assertTrue(self.check(t, values))

    @unittest.skip('Debugging')
    def test_remove1(self):
        pass

    @unittest.skip('Debugging')
    def test_find1(self):
        pass


if __name__ == '__main__':
    unittest.main()
