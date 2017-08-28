import unittest


def arrays(i):
    lines = i.split("\n")
    n = int(lines[0].strip())
    lines = lines[1:]

    result = []
    arr = map(int, lines[0].strip().split())
    for j in arr:
        result.insert(0, j)
    return ' '.join(map(str, result))


# def arrays():
#     n = int(input().strip())
#     arr = [int(arr_temp) for arr_temp in input().strip().split(' ')]



class TestArrays(unittest.TestCase):
    def test1(self):
        i = """4
        1 4 3 2"""

        out = """2 3 4 1"""

        self.assertEqual(arrays(i), out)


if __name__ == '__main__':
    unittest.main()
