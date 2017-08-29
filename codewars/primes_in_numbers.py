import unittest
from itertools import count, groupby


def primes(n):
    primes = [2]
    yield 2
    for i in range(2, int(n ** .5)):
        if any(i % j == 0 for j in primes):
            continue
        else:
            primes.append(i)
            yield i
    return None



def primeFactors(n):
    prime_iter = primes(n)
    factors = []
    i = next(prime_iter)
    while True:
        try:
            if n % i == 0:
                factors.append(i)
                n = int(n / i)
            else:
                i = next(prime_iter)
        except StopIteration:
            break

    if n != 1: factors.append(n)

    factor_info = map(lambda i: (i[0], len(list(i[1]))), groupby(factors))
    factor_strings = map(lambda i: '(%s%s)' % (i[0], '**%s' % i[1] if i[1] > 1 else ''), factor_info)
    return ''.join(factor_strings)


class TestPrimeFactors(unittest.TestCase):
    def test1(self):
        self.assertEqual(primeFactors(7775460), "(2**2)(3**3)(5)(7)(11**2)(17)")


if __name__ == '__main__':
    unittest.main()
