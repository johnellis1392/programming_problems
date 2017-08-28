import unittest
import random
import string


def anagrams2(s1, s2):
    def count(c, s):
        return len(list(filter(lambda i: i == c, s)))

    for c in s1:
        if count(c, s1) != count(c, s2):
            return False

    return True


def anagrams(s1, s2):
    letters = {}
    for c in s1:
        if c in letters:
            letters[c] += 1
        else:
            letters[c] = 1

    for c in s2:
        if c not in letters:
            return False
        elif letters[c] == 0:
            return False
        else:
            letters[c] -= 1

    return True


class TestAnagrams(unittest.TestCase):

    def randomString(self):
        return ''.join(random.choices(string.ascii_letters + string.digits, k = 20))

    # NOTE: Can also use random.shuffle() here
    def randomAnagram(self, s):
        return ''.join(random.sample(s, len(s)))

    def test1(self):
        input_data = ['asdf', 'fdsa']
        self.assertTrue(anagrams(*input_data))

    def test2(self):
        input_data = ['asdf', 'fdsaf']
        self.assertFalse(anagrams(*input_data))


    def test3(self):
        for _ in range(20):
            s = self.randomString()
            anagram = self.randomAnagram(s)
            self.assertTrue(anagrams(s, anagram))


if __name__ == '__main__':
    unittest.main()
