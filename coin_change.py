import unittest
import sys
from unittest.mock import patch
from functools import reduce

# Import IPython Debugger:
# https://github.com/gotcha/ipdb
# https://stackoverflow.com/questions/16867347/step-by-step-debugging-with-ipython
#
# List of commands:
# http://www.georgejhunt.com/olpc/pydebug/pydebug/ipdb.html
#
# NOTE:
# * c => Continue
# * n => Next Line
# * s => Step into method at point
# * l => List source code
# * p => Print result of expression
# * h => Help
import ipdb
# Set number of lines shown to 5
# ipdb.set_trace(context = 5)


def dump_cache(cache):
    sys.stdout.write("%s\n\n" % str(cache))

def dump_seqs(seq):
    for i in seq:
        sys.stdout.write("%s\n" % str(i))
    sys.stdout.write("\n")

def coin_change2():

    def getWays(n, c):
        cache = { i: 0 for i in range(0, n + 1) }

        # Populate cache with obvious answers
        for i in c: cache[i] = 1

        # Now calculate all solutions.
        # For every value up to the sum, calculate
        # the number of solutions.
        for i in range(min(c), n + 1):
            # cache[i] = 0
            if i in cache: continue

            # For every coin in the given list,
            # calculate the total number of
            # solutions in terms of all the
            # available coins.
            # cache[i] = reduce(lambda acc, k: acc + cache[k] if k in cache else acc, c, 0)
            cache[i] = 0
            for j in filter(lambda k: k <= i, c):
                cache[i] = cache[i] + cache[i - j]

            dump_cache(cache)

        return cache[n]


    n, m = map(int, input().strip().split())
    c = list(map(int, input().strip().split()))
    ways = getWays(n, c)
    print(str(ways))


def coin_change():
    def getWays(n, c):
        ipdb.set_trace(context = 7)
        if len(list(filter(lambda i: i < n, c))) == 0:
            # sys.stdout.write("%s\n" % str(n))
            # sys.stdout.write("%s\n" % str(c))
            # sys.stdout.write("%s\n" % str(list(filter(lambda i: i < n, c))))
            if n in c: return set([tuple([n])])
            else: return set([])

        # sys.stdout.write("%s\n" % str(n))
        sequences = set()
        for i in c:
            if i < n:
                seq = getWays(n - i, c)
                if n in c: seq.add(tuple([n]))
                # sequences.add(seq)
                sequences = sequences.union(seq)
            else:
                continue

        return sequences

    n, m = map(int, input().strip().split())
    c = list(map(int, input().strip().split()))
    ways = getWays(n, c)
    # print(str(ways))
    dump_seqs(ways)
    print(str(len(ways)))
        


class TestCoinChange(unittest.TestCase):

    @patch('__main__.input', side_effect = ['4 3', '1 2 3'])
    @patch('__main__.print')
    def test1(self, print_stub, input_stub):
        expected = '4'
        coin_change()
        print_stub.assert_called_once_with(expected)


    # @patch('__main__.input', side_effect = ['10 4', '2 5 3 6'])
    # @patch('__main__.print')
    # def test2(self, print_stub, input_stub):
    #     expected = '5'
    #     coin_change()
    #     print_stub.assert_called_once_with(expected)


if __name__ == '__main__':
    unittest.main()

