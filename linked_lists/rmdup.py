import unittest


# Using buffer
def rmdup2(s):
    l2 = []
    for c in s:
        if c in l2:
            continue
        else:
            l2.append(c)
    return ''.join(l2)


# With set (for better time complexity)
def rmdup3(s):
    chars = set()
    l2 = []
    for c in s:
        if c in chars:
            continue
        else:
            chars.add(c)
            l2.append(c)
    return ''.join(l2)


def rmdup(s):
    l = list(s)
    i = 0
    while i < len(l):
        j = i + 1
        while j < len(l):
            if l[i] == l[j]:
                l = l[:j] + l[j + 1:]
            else:
                j += 1
        i += 1
    return ''.join(l)


class TestRemoveDuplicates(unittest.TestCase):

    def test1(self):
        self.assertEqual(rmdup(''), '')
        self.assertEqual(rmdup('a'), 'a')
        self.assertEqual(rmdup('asdf'), 'asdf')

    def test2(self):
        self.assertEqual(rmdup('asdfasdf'), 'asdf')
        self.assertEqual(rmdup('asdfghjklqwertyuiopa'), 'asdfghjklqwertyuiop')

    def test3(self):
        self.assertEqual(rmdup('aaaaaaaaaaaaaaaaaaaaaaaaaaa'), 'a')



if __name__ == '__main__':
    unittest.main()

