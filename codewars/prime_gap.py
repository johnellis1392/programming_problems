import unittest
from itertools import count, dropwhile, takewhile


def primes():
    iterator = iter(count())
    next(iterator) # Skip 0
    next(iterator) # Skip 1
    yield next(iterator)
    primes = [2]

    for i in iterator:
        if any(i % j == 0 for j in primes):
            continue
        else:
            primes.append(i)
            yield i


class TestPrimes(unittest.TestCase):
    def test1(self):
        self.assertEqual(list(takewhile(lambda i: i <= 20, primes())), [2, 3, 5, 7, 11, 13, 17, 19])


def gap(g, m, n):
    prime_range = takewhile(lambda i: i <= n, dropwhile(lambda i: i < m, primes()))
    iterator = iter(prime_range)
    i = next(iterator)

    for j in iterator:
        if j - i == g:
            return [i, j]
        else:
            i = j
            continue

    return None


class TestGap(unittest.TestCase):
    def test1(self):
        self.assertEqual(gap(2,100,110), [101, 103])

    def test2(self):
        self.assertEqual(gap(4,100,110), [103, 107])

    def test3(self):
        self.assertEqual(gap(6,100,110), None)

    def test4(self):
        self.assertEqual(gap(8,300,400), [359, 367])

    def test5(self):
        self.assertEqual(gap(10,300,400), [337, 347])



if __name__ == '__main__':
    unittest.main()
