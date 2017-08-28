import unittest

from collections import deque
from itertools import chain, islice, permutations


num_checks = 0
def lena_sort(nums):
    global num_checks
    num_checks = 0

    def helper(nums):
        global num_checks
        if len(nums) <= 1: return nums
        pivot = nums[0]
        less = deque()
        more = deque()
        for i in islice(nums, 1, len(nums)):
            num_checks += 1
            if i < pivot: less.append(i)
            else: more.append(i)
        return chain(helper(less), [pivot], helper(more))

    return list(helper(deque(nums)))
    # print("Num Checks for %s: %d" % (str(nums), num_checks))


# l = list(range(5))
# l = list(range(10))
# l = list(range(7))
def f1(l):
    # checks = deque()
    checks = {}
    print()
    print("Values:")
    for i in permutations(l):
        lena_sort(i)
        if str(i) not in checks:
            checks[str(num_checks)] = i
        # checks.append((num_checks, i))

    _min = min(checks, key = lambda i: i[0])
    _max = max(checks, key = lambda i: i[0])

    # print("Minimum: \t%s \t%d" % (str(_min[1]), _min[0]))
    # print("Maximum: \t%s \t%d" % (str(_max[1]), _max[0]))
    # for i in sorted(checks, key = lambda i: i[0]):
    #     print("%d\t%s" % (i[0], str(i[1])))

    for k, v in reversed(sorted(checks.items(), key = lambda i: i[0])):
        print("%s\t%s" % (k, str(v)))

    return checks

def f2(n, m = None):
    m = m or n
    for i in range(n, m + 1):
        checks = f1(list(range(i)))
        # print("With N = %d" % i)
        # f1(list(range(i)))
        # print()
        # print("Values: ")
        # for i in sorted(check_list, key = lambda i: i[0]):
        #     print("%d, %s" % (i[0], str(i[1])))

# f2(8)
f1(list(range(8)))



# if __name__ == '__main__':
#     unittest.main()
