import unittest
import random


class Stack():

    def __init__(self):
        self.values = []

    def push(self, v):
        self.values.append(v)

    def pop(self):
        if len(self.values) == 0:
            return None
        else:
            return self.values.pop()

    def isempty(self):
        return len(self.values) == 0

    def peek(self):
        if len(self.values) == 0:
            return None
        else:
            return self.values[-1]

    def tolist(self):
        return self.values


def stack_sort2(stack):
    if stack.isempty():
        return stack

    v = stack.pop()
    if stack.isempty():
        stack.push(v)
        return stack

    stack_p = stack_sort(stack)
    if stack.peek() < v:
        w = stack.pop()
        stack.push(v)
        stack.push(w)
    else:
        stack.push(v)

    return stack


def stack_sort(stack):
    if stack.isempty(): return stack
    d = deque()
    result = deque()
    while len(stack) > 0:
        i = stack.pop()

        while len(stack) > 0:
            j = stack.pop()
            d.append(max(i, j))
            i = min(i, j)

        while len(d) > 0:
            stack.push(d.pop())

        result.append(i)

    for i in result:
        stack.push(i)

    return stack



class TestStackSort(unittest.TestCase):

    def random_stack(self):
        n = random.randint(10, 50)
        v = [ random.randint(0, 10000) for _ in range(n) ]
        s = Stack()
        for i in v: s.push(i)
        return s, v, n


    def test1(self):
        s = Stack()
        s.push(1)
        s.push(2)
        s.push(3)
        s_p = stack_sort(s)
        self.assertEqual(s_p.pop(), 1)
        self.assertEqual(s_p.pop(), 2)
        self.assertEqual(s_p.pop(), 3)
        self.assertTrue(s_p.isempty())


    def test2(self):
        for _ in range(20):
            s, v, n = self.random_stack()
            a = s.tolist()
            self.assertEqual(list(sorted(v)), a)



if __name__ == '__main__':
    unittest.main()
