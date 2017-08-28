import unittest


def unique2(s):
    chars = set()
    for c in s:
        if c in chars:
            return False
        else:
            chars.add(c)
    return True


def unique(s):
    for i in range(len(s)):
        for j in range(i + 1, len(s)):
            if s[i] == s[j]:
                return False
    return True



class TestUniqueChars(unittest.TestCase):

    def test1(self):
        input_data = 'asdfgqwerty'
        self.assertTrue(unique(input_data))


    def test2(self):
        input_data = 'asdfga'
        self.assertFalse(unique(input_data))

    
    def test3(self):
        input_data = 'a'
        self.assertTrue(unique(input_data))


if __name__ == '__main__':
    unittest.main()

