"""
Python3 Implementation for SetOfStacks
"""

__all__ = ['SetOfStacks']
__version__ = '0.1'
__author__ = 'John Ellis'


import collections
import random
import unittest


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
        y_index = 0

        # Remove all elements in ith stack and place in temporary stack
        while y_index < j:
            temp_stack1.append(stack1.pop())
            y_index += 1
        value = stack1.pop()

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

        if not self.stacks[-1]:
            self.stacks.pop()

        return value


class TestSetOfStacks(unittest.TestCase):
    """
    Unit Tests for SetOfStacks
    """

    @unittest.skip('Debugging')
    def test1(self):
        """
        Test simple case with SetOfStacks
        """
        pass


if __name__ == '__main__':
    unittest.main()
