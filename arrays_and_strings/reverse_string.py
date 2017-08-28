import unittest


def reverse_string(s):
    l = list(s)
    n = len(l)
    for i in range(int(n / 2)):
        c = l[i]
        l[i] = l[n - i - 1]
        l[n - i - 1] = c
    return ''.join(l)


class TestReverseString(unittest.TestCase):
    
    def test1(self):
        input_data = 'a'
        self.assertEqual(reverse_string(input_data), input_data)

    def test2(self):
        input_data = 'asdfqwerhjklyiuop'
        self.assertEqual(reverse_string(input_data), ''.join(reversed(input_data)))

    def test3(self):
        input_data = ''
        self.assertEqual(reverse_string(input_data), input_data)

    def test4(self):
        input_data = 'asdf'
        self.assertEqual(reverse_string(input_data), 'fdsa')

    def test5(self):
        input_data = 'asdfg'
        self.assertEqual(reverse_string(input_data), 'gfdsa')


if __name__ == '__main__':
    unittest.main()

