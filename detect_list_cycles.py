import unittest
import random
# from linked_list import List, Node
import linked_list
from ipdb import set_trace as debug


def detect_list_cycles(l):
    visited = set()
    node = l.head
    while node != None:
        if node in visited:
            return node
        else:
            visited.add(node)
            node = node.next
    return None


class TestDetectListCycles(unittest.TestCase):

    def random_cyclical_list(self):
        upper = 50
        lower = 10
        n = random.randint(lower, upper)
        l = linked_list.List([ random.randint(0, 10000) for _ in range(n) ])

        # Get last element in list
        node = l.head
        while node.next != None:
            node = node.next
        last = node

        # Get k'th element from list
        k = random.randint(1, n - 1)
        node = l.head
        for _ in range(k):
            node = node.next

        # Assign last element's next node to element k
        last.next = node

        return l, node


    def test1(self):
        l = linked_list.List([1, 2, 3])
        last = l.head.next.next
        k = l.head.next
        last.next = k

        actual = detect_list_cycles(l)
        self.assertTrue(k is actual)


    # @unittest.skip('Debugging')
    def test2(self):
        for _ in range(20):
            l, expected_node = self.random_cyclical_list()
            actual = detect_list_cycles(l)
            self.assertTrue(expected_node is actual)



if __name__ == '__main__':
    unittest.main()
