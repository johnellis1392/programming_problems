import unittest
import random


# Utility function for generating random test data
def random_tree(range_bounds = (10, 20), value_bounds = (0, 1e6)):
    t = Tree()
    i, j = range_bounds
    x, y = value_bounds
    n = random.randint(i, j)

    values = [ random.randint(x, y) for _ in range(n) ]
    for k in values: t.add(k)
    return t, n, values



class Node():

    def __init__(self, v):
        self.value = v
        self.left = None
        self.right = None


class Tree():

    def __init__(self):
        self.root = None

    def __insert__(self, v, n):
        if not n:
            return Node(v)
        elif v < n.value:
            n.left = self.__insert__(v, n.left)
        else:
            n.right = self.__insert__(v, n.right)
        return n

    def insert(self, v):
        return self.__insert__(self.root, v)



class TestTree(unittest.TestCase):

    def test_insert1(self):
        pass


    def test_remove1(self):
        pass


if __name__ == '__main__':
    unittest.main()
