import unittest
import re
from itertools import repeat, chain
from functools import reduce


def inflate_spaces(line, width):
    words = line.split()
    # Regular expressions
    # words = re.split(" +", line)
    num_spaces = width - sum(map(lambda w: len(w), words))
    num_gaps = len(words) - 1

    inflate_interval = lambda i: ' ' * (int(num_spaces / num_gaps) + 1 * (i <= num_spaces % num_gaps))
    return reduce(lambda acc, i: acc + inflate_interval(i[0]) + i[1], enumerate(words[1:], start = 1), words[0])


class TestInflateSpaces(unittest.TestCase):
    def test1(self):
        line = 'a b c d'
        width = 10
        result = inflate_spaces(line, width)
        self.assertEqual(result, 'a  b  c  d')

    def test2(self):
        line = 'a b c d'
        width = 14
        result = inflate_spaces(line, width)
        self.assertEqual(result, 'a    b   c   d')



class TestSplitLines(unittest.TestCase):
    def test1(self):
        self.assertEqual(split_lines('123 45 6', 7), ['123 45', '6'])


def justify(text, width):
    words = text.split()
    lines = []
    line = words[0]
    words = words[1:]

    for word in words:
        if len(line) + len(word) + 1 > width:
            lines.append(line)
            line = word
        else:
            line += ' ' + word

    lines = map(lambda l: inflate_spaces(l, width), lines)
    lines.append(line)
    return "\n".join(lines)



class TestJustify(unittest.TestCase):
    def test1(self):
        self.assertEqual(justify('123 45 6', 7), '123 45\n6')

    def test2(self):
        self.assertEqual(justify('consectetur adipiscing elit.', 30), 'consectetur  adipiscing  elit.')

    # def test3(self):
    #     self.assertEqual(justify('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum sagittis dolor mauris at elementum ligula', 30), "Lorem  ipsum  dolor  sit amet,\nconsectetur  adipiscing  elit.\nVestibulum    sagittis   dolor\nmauris,  at  elementum  ligula")

# unittest documentation:
# https://docs.python.org/2/library/unittest.html
# self.assertTrue(...)
# self.assertFalse(...)
# with self.assertRaises(TypeError):
#   s.split(2)

if __name__ == '__main__':
    unittest.main()
