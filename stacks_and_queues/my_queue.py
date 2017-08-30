import unittest
import random
from collections import deque


class MyQueue():

    def __init__(self):
        self.s1 = deque()
        self.s2 = deque()


    def enqueue(self, v):
        self.s1.append(v)
        return self


    def dequeue(self):
        while len(self.s1) != 0:
            self.s2.append(self.s1.pop())
        if len(self.s2) == 0: return None
        v = self.s2.pop()
        while len(self.s2) != 0:
            self.s1.append(self.s2.pop())
        return v


    def isempty(self):
        return len(self.s1) == 0


    def __len__(self):
        return len(self.s1)



class TestMyQueue(unittest.TestCase):

    MIN = 10
    MAX = 50
    VMIN = 0
    VMAX = 1e6

    def random_queue(self, range_bounds = (MIN, MAX), value_bounds = (VMIN, VMAX)):
        m, n = range_bounds
        i, j = value_bounds
        k = random.randint(m, m)
        values = [ random.randint(i, j) for _ in range(k)]
        queue = MyQueue()
        for l in values: queue.enqueue(l)
        return queue, n, values


    def test_enqueue1(self):
        queue = MyQueue()
        self.assertEqual(len(queue), 0)
        queue.enqueue(1)
        self.assertEqual(len(queue), 1)
        queue.enqueue(2)
        self.assertEqual(len(queue), 2)
        queue.enqueue(3)
        self.assertEqual(len(queue), 3)



    def test_dequeue1(self):
        queue = MyQueue()
        queue.enqueue(1)
        queue.enqueue(2)
        queue.enqueue(3)
        self.assertEqual(len(queue), 3)

        v = queue.dequeue()
        self.assertEqual(v, 1)
        self.assertEqual(len(queue), 2)
        v = queue.dequeue()
        self.assertEqual(v, 2)
        self.assertEqual(len(queue), 1)
        v = queue.dequeue()
        self.assertEqual(v, 3)
        self.assertEqual(len(queue), 0)


if __name__ == '__main__':
    unittest.main()
