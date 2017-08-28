import unittest
import random
import string


def sanitize(s):
    l = list(s)
    letters = { ' ': '%20' }
    i = 0
    while i < len(l):
        if l[i] in letters:
            c = l[i]
            l[i:i + 1] = letters[c]
            i += len(letters[c])
        else:
            i += 1
    return ''.join(l)


class TestSanitize(unittest.TestCase):

    def gen_random_strings(self):
        words = []
        letters = string.ascii_letters + string.digits
        for _ in range(random.randint(5, 20)):
            word_length = random.randint(5, 10)
            words.append(''.join(random.choices(letters, k = word_length)))
        return ' '.join(words), '%20'.join(words)

    def test1(self):
        for _ in range(20):
            sentence, sanitized_sentence = self.gen_random_strings()
            self.assertEqual(sanitized_sentence, sanitize(sentence))



if __name__ == '__main__':
    unittest.main()
