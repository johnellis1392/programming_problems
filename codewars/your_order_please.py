import unittest
import re


def order2(sentence):
    return ' '.join(sorted(sentence.split(), key = lambda i: int(filter(str.isdigit, i))))

def order(sentence):
    words = sentence.split()
    return ' '.join(sorted(words, key = lambda i: int(re.sub(r'[^0-9]', '', i))))


class TestOrder(unittest.TestCase):
    def test1(self):
        self.assertEqual(order("is2 Thi1s T4est 3a"), "Thi1s is2 3a T4est")


if __name__ == '__main__':
    unittest.main()
