test_cases = [
    'mjqjpqmgbljsphdztnvjfqwrcgsmlb',
    'bvwbjplbgvbhsrlpgdmjqwftvncz',
    'nppdvjthqldpwncqszvftbrmjlhg',
    'nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg',
    'zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw'
]


def readfile(filename):
    s = None
    with open(filename) as f:
        s = f.readline().strip()
    return s


def day06_part1(s):
    n = 4
    for i in range(len(s)-n):
        cs = set(s[i:i+n])
        if len(cs) == n:
            return i + n


def day06_part2(s):
    n = 14
    for i in range(len(s)-n):
        cs = set(s[i:i+n])
        if len(cs) == n:
            return i + n


if __name__ == '__main__':
    input_filename = 'input.txt'
    testing = False

    if testing:
        print('Part 1:')
        for i, test in enumerate(test_cases):
            r = day06_part1(test)
            print('- Test Case %s: %s' % (i, r))
        print()
        print('Part 2:')
        for i, test in enumerate(test_cases):
            r = day06_part2(test)
            print('- Test Case %s: %s' % (i, r))
    else:
        s = readfile(input_filename)
        result1 = day06_part1(s)
        print('Result1: %s' % result1)
        result2 = day06_part2(s)
        print('Result2: %s' % result2)
