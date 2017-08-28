import unittest


def coins(s, c):
    cache = {}
    cache[0] = 0
    for i in range(1, s + 1):
        if i in c:
            cache[i] = i
        else:
            pass



class TestCoins(unittest.TestCase):
    def test1(self):
        c = [1,3,5]
        s = 10
        expected = [5, 5]
        self.assertEqual(expected, coins(s, c))


if __name__ == '__main__':
    unittest.main()
