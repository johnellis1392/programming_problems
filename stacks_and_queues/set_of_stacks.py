import unittest
import random
import math

# Function for inspecting functions and classes:
# You can use `signature(lambda i: i * 2).parameters` to
# get a dictionary of values representing the parameter
# mappings of the function.
from inspect import signature, Signature
from collections import deque
from ipdb import set_trace as debug


class SetOfStacks():

    def __init__(self, capacity):
        self.stacks = deque()
        self.stacks.append(deque())
        self.capacity = capacity

    def push(self, v):
        stack = self.stacks[-1]
        if len(stack) == self.capacity:
            stack = deque()
            self.stacks.append(stack)
        stack.append(v)

    def pop(self):
        stack = self.stacks[-1]
        v = stack.pop()
        if len(stack) == 0 and len(self.stacks) > 1:
            self.stacks.pop()
        return v


    def isempty(self):
        return len(self.stacks) == 1 and len(self.stacks[0]) == 0

    def popat(self, index):
        pass

    def peek(self):
        if self.isempty():
            raise IndexError
        else:
            return self.stacks[-1][-1]

    def __len__(self):
        l = 0
        for i in self.stacks:
            l += len(i)
        return l


class TestSetOfStacks(unittest.TestCase):

    capacity = 12

    def random_stack(range_bounds = (10, 50), number_bounds = (0, 10000), capacity = self.capacity):
        m, n = range_bounds
        i, j = number_bounds
        stack = SetOfStacks(capacity)
        n = random.randint(m, n)
        for _ in range(n):
            stack.push(random.randint(i, j))
        return stack, n


    # @unittest.skip('Debugging')
    def test1(self):
        stack = SetOfStacks(self.capacity)
        stack.push(1)
        stack.push(2)
        stack.push(3)
        self.assertEqual(len(stack), 3)
        stack.pop()
        self.assertEqual(len(stack), 2)


    # @unittest.skip('Debugging')
    def test_push1(self):
        for _ in range(20):
            # stack = SetOfStacks(self.capacity)
            # n = random.randint(10, 50)
            # for _ in range(n):
            #     stack.push(random.randint(0, 10000))
            stack, n = self.random_stack(range_bounds = (10, 50))
            # debug(context = 10)

            self.assertEqual(len(stack), n)
            self.assertEqual(len(stack.stacks), math.ceil(n / self.capacity))
            self.assertEqual(len(stack.stacks[-1]), n % self.capacity)


    def test_push2(self):
        for _ in range(20):
            size = 24
            stack, n = self.random_stack(range_bounds = (size, size + 1))
            self.assertEqual(len(stack), size)
            self.assertEqual(len(stack.stacks), 2)
            self.assertEqual(len(stack.stacks[0]), self.capacity)
            self.assertEqual(len(stack.stacks[1]), self.capacity)

            for i in range(int(size / 2)):
                stack.pop()
            self.assertEqual(len(stack), int(size / 2))
            self.assertEqual(len(stack.stacks), 1)
            self.assertEqual(len(stack.stacks[0]), self.capacity)


    def test_pop1(self):
        stack = SetOfStacks(self.capacity)
        self.assertEqual(len(stack.stacks), 1)
        self.assertEqual(len(stack.stacks[0]), 0)
        # debug(context = 10)
        stack.push(1)
        self.assertEqual(len(stack.stacks), 1)
        self.assertEqual(len(stack.stacks[0]), 1)
        v = stack.pop()
        self.assertEqual(v, 1)
        self.assertEqual(len(stack.stacks), 1)
        self.assertEqual(len(stack.stacks[0]), 0)


    def test_pop2(self):
        for _ in range(20):
            size = 24
            stack, n = self.random_stack(range_bounds = (size, size + 1))
            self.assertEqual(len(stack.stacks), 2)

            for i in range(int(size / 2)):
                stack.pop()
            self.assertEqual(len(stack), int(size / 2))
            self.assertEqual(len(stack.stacks), 1)
            self.assertEqual(len(stack.stacks[0]), self.capacity)


    @unittest.skip('Debugging')
    def test_peek1(self):
        stack = SetOfStacks(self.capacity)
        n = random.randint(10, 50)
        v = None
        for _ in range(n):
            v = random.randint(0, 10000)
            stack.push(v)
        self.assertEqual(stack.peek(), v)


    @unittest.skip('Debugging')
    def test_isempty1(self):
        stack = SetOfStacks(self.capacity)
        self.assertTrue(stack.isempty())
        stack.push(1)
        self.assertFalse(stack.isempty())
        stack.pop()
        # debug(context = 10)
        self.assertTrue(stack.isempty())


if __name__ == '__main__':
    unittest.main()
