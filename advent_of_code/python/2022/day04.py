import re


def day04_part1(file):
    s = 0
    with open(file) as f:
        for row in f.readlines():
            m = re.search(r'(\d+)-(\d+),(\d+)-(\d+)', row.strip())
            a1, a2, b1, b2 = m.group(1), m.group(2), m.group(3), m.group(4)
            # print(a1, a2, b1, b2)
            a = set(range(int(a1), int(a2)+1))
            b = set(range(int(b1), int(b2)+1))
            if a.issubset(b) or b.issubset(a):
                s += 1
    return s


def day04_part2(file):
    s = 0
    with open(file) as f:
        for row in f.readlines():
            m = re.search(r'(\d+)-(\d+),(\d+)-(\d+)', row.strip())
            a1, a2, b1, b2 = m.group(1), m.group(2), m.group(3), m.group(4)
            a = set(range(int(a1), int(a2)+1))
            b = set(range(int(b1), int(b2)+1))
            if len(a.intersection(b)) != 0:
                s += 1
    return s


if __name__ == '__main__':
    # input_filename = 'input.test.txt'
    input_filename = 'input.txt'
    result1 = day04_part1(input_filename)
    print('Result1 = %s' % result1)
    result2 = day04_part2(input_filename)
    print('Result2 = %s' % result2)
