import unittest
# from itertools import groupby

# You are given an array (which will have a length of at least 3,
# but could be very large) containing integers. The array is
# either entirely comprised of odd integers or entirely comprised
# of even integers except for a single integer N. Write a method
# that takes the array as an argument and returns N.
#
# For example:
#
# [2, 4, 0, 100, 4, 11, 2602, 36]
#
# Should return: 11
#
# [160, 3, 1719, 19, 11, 13, -21]
#
# Should return: 160


def groupby(values, keyfunc):
    result = {}
    for i in values:
        key = keyfunc(i)
        if key in result:
            result[key].append(i)
        else:
            result[key] = [i]
    return result


def find_outlier2(integers):
    # keyfunc = lambda i: if i % 2 == 0: 'even' else: 'odd'
    keyfunc = lambda i: 'even' if i % 2 == 0 else 'odd'
    groups = groupby(integers, keyfunc)
    if 'even' in groups and len(groups['even']) == 1:
        return groups['even'][0]
    elif 'odd' in groups and len(groups['odd']) == 1:
        return groups['odd'][0]
    else:
        raise ValueError('No outlier found')


def find_outlier(integers):
    evens = [i for i in integers if i % 2 == 0]
    odds = [i for i in integers if i % 2 != 0]
    return evens[0] if len(evens) == 1 else odds[0]


class TestFindOutliers(unittest.TestCase):
    def test1(self):
        self.assertEqual(find_outlier([2, 6, 8, 10, 3]), 3)

    def test2(self):
        self.assertEqual(find_outlier([1, 2, 3]), 2)

    def test3(self):
        test_input = list(range(0, 100, 2))
        test_input.insert(25, 11)
        self.assertEqual(find_outlier(test_input), 11)

    # def test4(self):
    #     with self.assertRaises(ValueError):
    #         find_outlier([1, 3, 5])


if __name__ == '__main__':
    unittest.main()
