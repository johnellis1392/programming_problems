import unittest
from itertools import tee, repeat, islice, chain
from functools import partial
from collections import deque


def crush(arr, a, b, k):
    for i in range(a - 1, b):
        arr[i] += k
    return arr


class TestCrush(unittest.TestCase):
    def test1(self):
        input_data = [0, 0, 0, 0, 0], 1, 2, 100
        expected = [100, 100, 0, 0, 0]
        self.assertEqual(crush(*input_data), expected)

    def test1(self):
        input_data = [100, 100, 0, 0, 0], 2, 5, 100
        expected = [100, 200, 100, 100, 100]
        self.assertEqual(crush(*input_data), expected)


# def temp(values, n):
#     arr = repeat(0, n)
#     def temp2(a, b, k, i):
#         # print(a, b, k, i)
#         if a - 1 <= i[0] and i[0] < b:
#             return k + i[1]
#         else:
#             return k
#
#     for a, b, k in values:
#         # arr = map(lambda i: (i[1] + k if a - 1 <= i[0] and i[0] < b else i[1]), enumerate(iter(arr)))
#         _, arr0 = tee(arr)
#         arr = map(lambda i: temp2(a, b, k, i), enumerate(arr0))
#     return max(arr)

# def temp(values, n):
#     # arr = deque(repeat(0, n))
#     arr = list(repeat(0, n))
#     for a, b, k in values:
#         arr[a - 1:b] = map(int.__add__, islice(arr, a - 1, b), repeat(k, b - a + 1))
#     return max(arr)


def temp(values, n):
    arr = deque(repeat(0, n))
    for a, b, k in values:
        arr = deque(chain(
            islice(arr, 0, a - 1),
            map(int.__add__, islice(arr, a - 1, b), repeat(k, b - a + 1)),
            islice(arr, b, n)
        ))
        # map(int.__add__, partial(islice, arr, a - 1, b), partial(repeat, k, b - a + 1)),
    return max(arr)


class TestTemp(unittest.TestCase):
    def test1(self):
        input_data = [(1, 2, 100), (2, 5, 100), (3, 4, 100)], 5
        expected = 200
        self.assertEqual(expected, temp(*input_data))


if __name__ == '__main__':
    unittest.main()
