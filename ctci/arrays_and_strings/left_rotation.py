import unittest

def left_rotation(a, d):
    return a[d:] + a[0:d]

# def left_rotation():
#     n, d = map(int, input().strip().split())
#     a = list(map(int, input().strip().split()))
#     result = left_rotation(a, d)
#     print(" ".join(map(str, result)))


class TestLeftRotation(unittest.TestCase):
    def test1(self):
        input_data = [1, 2, 3, 4, 5], 4
        expected = [5, 1, 2, 3, 4]
        self.assertEqual(expected, left_rotation(*input_data))


if __name__ == '__main__':
    unittest.main()
