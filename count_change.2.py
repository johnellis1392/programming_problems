import unittest
import sys
from unittest.mock import patch


# def __print(s):
#     sys.stdout.write("%s\n" % s)


# Recursive solution.
# NOTE: This solves the wrong problem; this is finding
# the lowest number of coins required, not finding the
# total number of solutions.
# def getWays(n, c):
#     if n <= 0:
#         # There are no solutions for 0
#         __print("Returning 0 for value: %d" % n)
#         return 0
#     elif n in c:
#         # If there is a coin with n's value,
#         # return 1 solution
#         __print("Returning 1 for value: %d" % n)
#         return 1
#     else:
#         solutions = []
#         for i in c:
#             s = getWays(n - i, c)
#             __print("Got solution %d for value %d" % (s, n - i))
#             if s > 0:
#                 solutions.append(i)
# 
#         # Return minimal solution if one exists
#         if len(solutions) > 0:
#             __print("Found solutions %s for %d; returning %d" % (str(solutions), n, min(solutions)))
#             return min(solutions) + 1
#         else:
#             __print("No solution found for %d, returning 0")
#             return 0


# REAL Recursive Solution.
# def getWays(n, c):
#     if n <= 0:
#         return []
# 
#     solutions = set()
#     if n in c:
#         solutions.add(tuple([n]))
# 
#     for i in c:
#         s = getWays(n - i, c)
#         s = set(map(lambda j: tuple(sorted(list(j) + [i])), s))
#         solutions = solutions.union(s)
# 
#     return solutions


# Memoized Recursive Solution
# cache = {}
# numExecutions = 0
# def getWays(n, c):
#     if n <= 0:
#         return []
# 
#     if n in cache:
#         return cache[n]
# 
#     global numExecutions
#     numExecutions += 1
#     __print("Num executions: %d" % numExecutions)
#     solutions = set()
#     if n in c:
#         solutions.add(tuple([n]))
# 
#     for i in c:
#         s = getWays(n - i, c)
#         s = set(map(lambda j: tuple(sorted(list(j) + [i])), s))
#         solutions = solutions.union(s)
# 
#     cache[n] = solutions
#     return solutions


# Bottom-Up Approach
def getWays(n, c):
    cache = { 0: 0 }
    
    for i in range(n + 1):
        solutions = set()
        if i in c: solutions.add(tuple([i]))

        for j in c:
            if (i - j) not in cache: continue
            s = cache[i - j]
            s_p = set(map(lambda k: tuple(sorted(list(k) + [j])), s))
            solutions = solutions.union(s_p)

        cache[i] = solutions

    return cache[n]


def coin_change():

    # The number of coins and the sum to calculate form a matrix
    # of posibilities, where the matrix represents the least
    # number of coins required to form the sum.
    # def getWays(n, c):
    #     pass

    n, m = map(int, input().strip().split())
    c = list(map(int, input().strip().split()))
    ways = getWays(n, c)
    # __print(str(ways))
    print(str(len(ways)))
    # print(str(ways))



class TestCoinChange(unittest.TestCase):

    @patch('__main__.input', side_effect = ['4 3', '1 2 3'])
    @patch('__main__.print')
    def test1(self, print_stub, input_stub):
        expected = '4'
        coin_change()
        print_stub.assert_called_once_with(expected)


    @patch('__main__.input', side_effect = ['10 4', '2 5 3 6'])
    @patch('__main__.print')
    def test2(self, print_stub, input_stub):
        expected = '5'
        coin_change()
        print_stub.assert_called_once_with(expected)


if __name__ == '__main__':
    unittest.main()

