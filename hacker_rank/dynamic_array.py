import unittest
import sys
# from ipdb import set_trace as debug
from unittest.mock import patch, Mock, call


def _print(s):
    sys.stdout.write("%s\n" % s)


def dynamic_array():
    n, q = map(int, input().strip().split())
    seqs = [[] for _ in range(n)]
    lastAnswer = 0

    for _ in range(q):
        t, x, y = map(int, input().strip().split())
        seq = seqs[(x ^ lastAnswer) % n]

        if t == 1:
            seq.append(y)
        elif t == 2:
            lastAnswer = seq[y % len(seq)]
            print(str(lastAnswer))
        else:
            raise ValueError('Unknown query type: %d' % t)


class TestDynamicArray(unittest.TestCase):

    input_data = """
    2 5
    1 0 5
    1 1 7
    1 0 3
    2 1 0
    2 1 1
    """


    @patch('__main__.input', side_effect = input_data.strip().split('\n'))
    @patch('__main__.print')
    def test1(self, print_stub, input_stub):
        dynamic_array()
        print_stub.assert_has_calls([call('7'), call('3')])


if __name__ == '__main__':
    unittest.main()
