"""
Python3 Implementation for SetOfStacks
"""

__all__ = ['SetOfStacks']
__version__ = '0.1'
__author__ = 'John Ellis'


import collections
import math
import random
import unittest

# import ipdb


def random_stack(capacity, range_bounds=(10, 50), number_bounds=(0, 10000)):
    """
    Generate random stack
    """
    i, j = range_bounds
    lower_bound, upper_bound = number_bounds
    stack = SetOfStacks(capacity)

    values = collections.deque()
    length = random.randint(i, j)
    for _ in range(length):
        value = random.randint(lower_bound, upper_bound)
        values.append(value)
        stack.push(value)
    return stack, values


def random_array(range_bounds=(10, 30), value_bounds=(1, 1e6)):
    """
    Generate random array for unit testing
    """
    i, j = range_bounds
    lower_bound, upper_bound = value_bounds
    length = random.randint(i, j)
    values = [random.randint(lower_bound, upper_bound) for _ in range(length)]
    return values


def fill_stack(stack1, stack2, capacity):
    """
    Fill contents of stack2 until either stack1 is expended
    or the capacity is reached
    """

    temp_stack1 = collections.deque()
    while stack1:
        temp_stack1.append(stack1.pop())
    while len(stack2) < capacity and stack1:
        stack2.append(temp_stack1.pop())
    while temp_stack1:
        stack1.append(temp_stack1.pop())
    return


class SetOfStacks():
    """
    Data structure representing a set of plates
    """

    def __init__(self, capacity=3):
        self.stacks = collections.deque()
        self.capacity = capacity

    def __len__(self):
        return sum(map(len, self.stacks))

    def __repr__(self):
        return '[\t%s ]' % '\n\t'.join(map(str, self.stacks))

    def push(self, value):
        """
        Push a value onto the latest substack
        """

        if not self.stacks:
            # No stacks are defined; create a new one
            self.stacks.append(collections.deque([value]))
        elif len(self.stacks[-1]) == self.capacity:
            # Last stack at capacity; create new stack
            self.stacks.append(collections.deque([value]))
        else:
            self.stacks[-1].append(value)
        return self

    def pop(self):
        """
        Pop the last element from the set of stacks
        """

        if not self.stacks:
            # No elements in set
            return None

        value = self.stacks[-1].pop()
        if not self.stacks[-1]:
            # No elements left in last stack; remove
            self.stacks.pop()

        return value

    def pop_at(self, index):
        """
        Pop an element from the set of stacks at a given index.
        """

        if index < 0 or index > len(self):
            # Invalid index given
            return None

        # Get indices of element
        i = int(index / self.capacity)
        j = int(index % self.capacity)

        stack1 = self.stacks[i]
        temp_stack1 = collections.deque()
        length = len(stack1)
        y_index = 0

        # Remove all elements in ith stack and place in temporary stack
        while y_index < length - j:
            temp_stack1.append(stack1.pop())
            y_index += 1
        # value = stack1.pop()
        value = temp_stack1.pop()

        # Fill all elements back into stack
        while temp_stack1:
            stack1.append(temp_stack1.pop())

        # For all substacks from i + 1 onward, take all extra
        # elements in next stack and fill previous stack
        stack_index = i + 1
        while stack_index < len(self.stacks):
            fill_stack(self.stacks[stack_index],
                       self.stacks[stack_index - 1],
                       self.capacity)
            stack_index += 1

        # ipdb.set_trace(context=25)
        if not self.stacks[i]:
            # self.stacks.pop()
            self.stacks = collections.deque(
                list(self.stacks)[:i] + list(self.stacks)[i:])

        return value

    def isempty(self):
        """
        Determine if the stack is empty or not
        """
        return len(self.stacks) == 0

    def peek(self):
        """
        Get top-most element of stack
        """
        if not self.stacks:
            return None

        return self.stacks[-1][-1]


class TestSetOfStacks(unittest.TestCase):
    """
    Unit Tests for SetOfStacks
    """

    capacity = 12

    # @unittest.skip('Debugging')
    def test1(self):
        """
        Test simple case with SetOfStacks
        """
        stack = SetOfStacks(self.capacity)
        stack.push(1)
        stack.push(2)
        stack.push(3)
        self.assertEqual(len(stack), 3)
        stack.pop()
        self.assertEqual(len(stack), 2)

    # @unittest.skip('Debugging')
    def test_push1(self):
        """
        Test simple case with SetOfStacks
        """

        for _ in range(20):
            stack, values = random_stack(self.capacity, range_bounds=(10, 50))
            length = len(values)

            self.assertEqual(len(stack), length)
            self.assertEqual(len(stack.stacks),
                             math.ceil(length / self.capacity))
            self.assertEqual(len(stack.stacks[-1]) % self.capacity,
                             length % self.capacity)

    def test_push2(self):
        """
        Test simple case with SetOfStacks
        """

        size = 24
        stack, _ = random_stack(self.capacity,
                                range_bounds=(size, size))
        self.assertEqual(len(stack), size)
        self.assertEqual(len(stack.stacks), 2)
        self.assertEqual(len(stack.stacks[0]), self.capacity)
        self.assertEqual(len(stack.stacks[1]), self.capacity)

        for _ in range(int(size / 2)):
            stack.pop()
        self.assertEqual(len(stack), int(size / 2))
        self.assertEqual(len(stack.stacks), 1)
        self.assertEqual(len(stack.stacks[0]), self.capacity)

    def test_pop1(self):
        """
        Test simple case with SetOfStacks
        """

        stack = SetOfStacks(self.capacity)
        self.assertEqual(len(stack.stacks), 0)
        stack.push(1)
        self.assertEqual(len(stack.stacks), 1)
        self.assertEqual(len(stack.stacks[0]), 1)
        value = stack.pop()
        self.assertEqual(value, 1)
        self.assertEqual(len(stack.stacks), 0)

    def test_pop2(self):
        """
        Test simple case with SetOfStacks
        """

        for _ in range(20):
            size = 24
            stack, original_values = random_stack(
                self.capacity,
                range_bounds=(size, size))
            self.assertEqual(len(stack.stacks), 2)

            for i in range(size - 1):
                value = stack.pop()
                position = size - i - 1
                self.assertEqual(original_values[position], value)
                self.assertEqual(len(stack), position)
                self.assertEqual(
                    len(stack.stacks),
                    math.ceil(position / self.capacity))
                self.assertEqual(
                    len(stack.stacks[-1]) % self.capacity,
                    int(position % self.capacity))

            self.assertEqual(len(stack), 1)
            self.assertEqual(stack.pop(), original_values[0])
            self.assertEqual(len(stack.stacks), 0)

    # @unittest.skip('Debugging')
    def test_peek1(self):
        """
        Test simple case with SetOfStacks
        """

        stack = SetOfStacks(self.capacity)
        length = random.randint(10, 50)
        value = None
        for _ in range(length):
            value = random.randint(0, 10000)
            stack.push(value)
        self.assertEqual(stack.peek(), value)

    # @unittest.skip('Debugging')
    def test_isempty1(self):
        """
        Test simple case with SetOfStacks
        """

        stack = SetOfStacks(self.capacity)
        self.assertTrue(stack.isempty())
        stack.push(1)
        self.assertFalse(stack.isempty())
        stack.pop()
        self.assertTrue(stack.isempty())

    @unittest.skip('Debugging')
    def test_pop_at1(self):
        """
        Test pop_at pops correct value
        """

        stack = SetOfStacks(self.capacity)
        for i in [1, 2, 3]:
            stack.push(i)
        self.assertEqual(len(stack), 3)

        for j in [1, 2, 3]:
            value = stack.pop_at(0)
            self.assertEqual(value, j)
        self.assertEqual(len(stack), 0)

    def test_pop_at2(self):
        """
        Test pop_at with random data
        """

        iterations = 10
        length = self.capacity * iterations
        stack, values = random_stack(
            self.capacity,
            range_bounds=(length, length))
        for i in range(iterations):
            self.assertEqual(len(stack.stacks), iterations - i)
            for j in range(self.capacity):
                value = stack.pop_at(0)
                self.assertEqual(value, values[i * self.capacity + j])
            print(stack)
            self.assertEqual(len(stack.stacks), iterations - i - 1)

        # stack, values = random_stack(
        #     self.capacity,
        #     range_bounds=(length, length))
        # for expected in values:
        #     value = stack.pop_at(0)
        #     print(value)
        #     self.assertEqual(expected, value)
        # self.assertEqual(0, len(stack))


if __name__ == '__main__':
    unittest.main()
