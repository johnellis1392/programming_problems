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

    def __repr__(self):
        left = id(self.left) if self.left else None
        right = id(self.right) if self.right else None
        return 'Node(value=%d, left=%s, right=%s)' % (self.value, str(left), str(right))


class BalancedTree():

    def __init__(self):
        self.root = None

    def __height__(self, n):
        if not n:
            return -1
        else:
            return 1 + max(self.__height__(n.left), self.__height__(n.right))

    def __rotateright__(self, n):
        n1 = n
        n2 = n.left
        t1 = n.left.right

        n2.right = n1
        n1.left = t1
        n = n2

        return n

    def __rotateleft__(self, n):
        n1 = n
        n2 = n.right
        t1 = n.right.left

        n2.left = n1
        n1.right = t1
        n = n2

        return n

    def __balancefactor__(self, n):
        if n == None:
            return 0
        else:
            # return self.__height__(n.left) - self.__height__(n.right)
            return self.__height__(n.right) - self.__height__(n.left)

    def __balance__(self, n):
        if n == None:
            return n

        balancefactor = self.__balancefactor__(n)
        # if balancefactor < -1:
        if balancefactor < 0:
            # Left heavy
            if self.__balancefactor__(n.left) > 0:
                n.left = self.__rotateleft__(n.left)
            n = self.__rotateright__(n)
        # elif balancefactor > 1:
        elif balancefactor > 0:
            # Right heavy
            if self.__balancefactor__(n.right) < 0:
                n.right = self.__rotateright__(n.right)
            n = self.__rotateleft__(n)
        else:
            # Balanced
            pass

        return n

    # def __balance__(self, n):
    #     if n == None:
    #         return None
    #
    #     hl = self.__height__(n.left)
    #     hr = self.__height__(n.right)
    #
    #     if abs(hl - hr) > 1:
    #         if hl > hr:
    #             # Left-heavy; rotate right
    #             if n.left.right != None:
    #                 n.left = self.__rotateleft__(n.left)
    #                 n = self.__rotateright__(n)
    #             else:
    #                 n = self.__rotateright__(n)
    #         else:
    #             # Right-heavy; rotate left
    #             if n.right.left != None:
    #                 n.right = self.__rotateright__(n.right)
    #                 n = self.__rotateleft__(n)
    #             else:
    #                 n = self.__rotateleft__(n)
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
        return self.__balance__(n)

    def insert(self, v):
        self.root = self.__insert__(v, self.root)
        return self

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

    # @unittest.skip('Debugging')
    def test_insert1(self):
        t = BalancedTree()
        n = 7
        v = []
        for i in range(1, n + 1):
            v.append(i)
            t.insert(i)

        h = t.height()
        logh = int(math.log(n, 2))
        self.assertEqual(len(t), n)
        self.assertTrue(logh <= h <= logh + 1)
        self.assertTrue(self.check(t, v))

    # @unittest.skip('Debugging')
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

    # @unittest.skip('Debugging')
    def test_insert3(self):
        for _ in range(50):
            t, n, v = random_balanced_tree()
            self.assertEqual(len(t), n)

            h = t.height()
            logh = math.floor(math.log(n, 2))
            debug_string = """

 * Test Failed:
 ** input: "%s"
 ** n = %d, h = %d, log(h, 2) = %f

 * Tree:\n%s
""" % (str(v), n, h, math.log(h, 2), t.debug_string())

            self.assertTrue(logh <= h <= logh + 1, msg=debug_string)
            self.assertTrue(self.check(t, v))

    # @unittest.skip('Debugging')
    def test_insert4(self):
        for i in range(1, 30 + 1):
            t = BalancedTree()
            v = list(range(1, i + 1))
            for j in v:
                t.insert(j)

            h = t.height()
            logh = int(math.log(i, 2))

            self.assertEqual(len(t), i)
            self.assertTrue(
                logh <= h <= logh + 1, msg='\n\ni: %d\nh: %d\nlogh: %f\nTree:\n%s\n\n' % (i, h, math.log(i, 2), t.debug_string()))
            self.assertTrue(self.check(t, v))

    # @unittest.skip('Debugging')
    def test_insert5(self):
        t = BalancedTree()
        values = [849403, 818924, 865206, 69780, 154144, 464240, 642839,
                  439150, 966005, 519811, 637830, 728303, 880476, 77348, 524712]
        n = len(values)
        for i in values:
            t.insert(i)

        h = t.height()
        logh = int(math.log(n, 2))
        self.assertEqual(len(t), n)
        self.assertTrue(logh <= h <= logh + 1)
        self.assertTrue(self.check(t, values))

    # @unittest.skip('Debugging')
    def test_insert6(self):
        t = BalancedTree()
        values = [11, 10, 12, 0, 2, 4, 8, 3, 14, 5, 7, 9, 13, 1, 6]
        n = len(values)
        for i in values:
            t.insert(i)

        h = t.height()
        logh = int(math.log(n, 2))
        self.assertEqual(len(t), n)

        self.assertTrue(logh <= h <= logh + 1)
        self.assertTrue(self.check(t, values))

    # @unittest.skip('Debugging')
    def test_insert7(self):
        t = BalancedTree()
        n = 14
        v = list(range(1, n + 1))
        for i in v:
            # print(' * Inserting %d' % i)
            t.insert(i)

        h = t.height()
        logh = int(math.log(n, 2))
        debug_string = """

 * Test Failed:
 ** input: "%s"
 ** n = %d, h = %d, log(h, 2) = %f

 * Tree:\n%s
""" % (str(v), n, h, math.log(h, 2), t.debug_string())

        self.assertEqual(len(t), n)
        self.assertTrue(logh <= h <= logh + 1, msg=debug_string)
        self.assertTrue(self.check(t, v))

    @unittest.skip('Debugging')
    def test_remove1(self):
        pass

    @unittest.skip('Debugging')
    def test_find1(self):
        pass


if __name__ == '__main__':
    unittest.main()
