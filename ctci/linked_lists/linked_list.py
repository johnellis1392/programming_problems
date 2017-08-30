import unittest
import collections
import random
from ipdb import set_trace as debug


class Node():
    def __init__(self, data):
        self.data = data
        self.next = None

    def __str__(self):
        return 'Node(%s)' % str(self.data)

    def __add__(self, other):
        return self.data + other.data

    def __radd__(self, other):
        return other.data + self.data

    def __lt__(self, other):
        return self.data < other.data

    def __le__(self, other):
        return self.data <= other.data

    def __eq__(self, other):
        # debug(context = 10)
        if type(other) == Node:
            return self.data == other.data
        else:
            return self.data == other

    def __ne__(self, other):
        if type(other) == Node:
            return self.data != other.data
        else:
            return self.data != other

    def __gt__(self, other):
        return self.data > other.data

    def __ge__(self, other):
        return self.data >= other.data

    # Function for allowing nodes to be hashed.
    # The repr function here returns a string describing
    # the object in terms of its type and memory location,
    # and the hash function converts this string into a
    # checksum.
    def __hash__(self):
        return hash(repr(self))


class List():

    def __init__(self, values = []):
        if not values or type(values) is not list or len(values) == 0:
            self.head = None
        else:
            node = self.head = Node(values[0])
            for i in values[1:]:
                node.next = Node(i)
                node = node.next


    def add(self, value):
        node = Node(value)
        if self.head == None:
            self.head = node
            return self

        n = self.head
        while n.next != None:
            n = n.next
        n.next = node
        return self


    def concat(self, l):
        l2 = List()
        n = self.head
        n2 = l2.head = Node(n.data)

        while n != None:
            n2.next = Node(n.data)
            n2 = n2.next
            n = n.next

        n = l.head
        while n != None:
            n2.next = Node(n.data)
            n2 = n2.next
            n = n.next

        return l2


    # Override implicit length function for len(...) calls
    def __len__(self):
        n = 0
        node = self.head
        while node != None:
            n += 1
            node = node.next
        return n


    class ListIterator():
        def __init__(self, node):
            self.current = node

        def __next__(self):
            try:
                result = self.current.data
                self.current = self.current.next
                return result
            except Exception:
                raise StopIteration


    # Override the iterator protocol using __iter__ and __next__ methods
    def __iter__(self):
        return self.ListIterator(self.head)

    # def __next__(self):
    #     pass


    # Override [] getter method
    def __getitem__(self, index):
        i = 0
        node = self.head
        while i < index:
            if node == None:
                raise IndexError
            node = node.next
            i += 1
        return node.data

    # Override []= setter method
    def __setitem__(self, index, value):
        pass

    # Get a reversed iterator for the reversed(...) builtin
    def __reversed__(self):
        pass


    def __add__(self, value):
        if type(value) is List:
            return self.concat(value)
        else:
            return self.add(value)


    def __eq__(self, other):
        if type(other) != List: return False
        elif len(self) != len(other): return False

        n1 = self.head
        n2 = other.head
        while n1 != None and n2 != None:
            if n1.data != n2.data:
                return None
            else:
                n1 = n1.next
                n2 = n2.next

        return True


    def __str__(self):
        values = []
        node = self.head
        while node != None:
            values.append(node.data)
            node = node.next
        return 'List([%s])' % ', '.join(map(str, values))




class TestList(unittest.TestCase):

    def random_list(self):
        n = random.randint(20, 50)
        l = List()
        values = collections.deque()
        for _ in range(n):
            value = random.randint(0, 1000)
            values.append(value)
            l.add(value)
        return l, n, values


    def test1(self):
        l = List().add(1).add(2).add(3)
        self.assertEqual(len(l), 3)


    # Test randomly-generated lists
    def test2(self):
        for _ in range(10):
            l, n, values = self.random_list()
            self.assertEqual(len(l), n)
            node = l.head
            i = 0
            while node:
                self.assertTrue(i < n)
                expected = Node(values[i])
                self.assertEqual(expected, node)
                node = node.next
                i += 1


    def test_iterator1(self):
        l, n, values = self.random_list()
        self.assertEqual(len(l), n)
        for i in range(n):
            self.assertEqual(l[i], values[i])

    def test_eq1(self):
        l1 = List().add(1).add(2).add(3)
        l2 = List().add(1).add(2).add(3)
        self.assertEqual(l1, l2)

    def test_eq2(self):
        for _ in range(10):
            l, _, _ = self.random_list()
            self.assertEqual(l, l)

    def test_init1(self):
        l = List([1, 2, 3])
        expected = List().add(1).add(2).add(3)
        self.assertEqual(expected, l)


if __name__ == '__main__':
    unittest.main()
