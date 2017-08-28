import unittest
from linked_list import List
import random
from ipdb import set_trace as debug

# Reload function for reloading a module
# from importlib import reload



def add_num_lists(l1, l2):
    n1 = l1.head
    n2 = l2.head
    carry = 0
    result = List()
    while n1 != None and n2 != None:
        value = n1.data + n2.data + carry
        result.add(value % 10)
        carry = int(value / 10)

        n1 = n1.next
        n2 = n2.next

    rest = n1 if n1 != None else n2
    while rest != None:
        value = rest.data + carry
        result.add(value % 10)
        carry = int(value / 10)
        rest = rest.next

    if carry != 0: result.add(carry)
    return result


class TestAddNumLists(unittest.TestCase):

    def to_list(self, n):
        l = List()
        for i in map(int, reversed(str(n))):
            l.add(i)
        return l

    def random_lists(self):
        lower = 100
        upper = 100000000
        n = random.randint(lower, upper)
        m = random.randint(lower, upper)
        l1 = self.to_list(n)
        l2 = self.to_list(m)
        return n, l1, m, l2


    def test1(self):
        l1 = List([1, 2, 3])
        l2 = List([1, 2, 3])
        expected = List([2, 4, 6])

        actual = add_num_lists(l1, l2)
        self.assertEqual(expected, actual)


    def test2(self):
        for _ in range(20):
            n, l1, m, l2 = self.random_lists()
            expected = self.to_list(n + m)
            # debug(context = 10)

            actual = add_num_lists(l1, l2)
            # print('L1 (%d): %8s\nL2 (%d): %8s\nExpected: %8s\nActual: %8s\n' % (n, str(l1), m, str(l2), str(expected), str(actual)))
            self.assertEqual(expected, actual)



if __name__ == '__main__':
    unittest.main()
