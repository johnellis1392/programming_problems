import unittest


def sparse_arrays(a, b):
    return list(map(lambda i: len(list(filter(lambda j: j == i, a))), b))


class TestSparseArrays(unittest.TestCase):
    def test1(self):
        input_data = ['aba', 'baba', 'aba', 'xzxb'], ['aba', 'xzxb', 'ab']
        expected = [2, 1, 0]
        self.assertEqual(sparse_arrays(*input_data), expected)


if __name__ == '__main__':
    unittest.main()
