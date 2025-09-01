def priority(a):
    if a in 'abcdefghijklmnopqrstuvwxyz':
        return ord(a) - ord('a') + 1
    elif a in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
        return ord(a) - ord('A') + 1 + 26


def day03_part1(file):
    total = 0
    with open(file) as f:
        for row in f.readlines():
            n = len(row) // 2
            r1, r2 = set(row[:n]), set(row[n:])
            a = r1.intersection(r2).pop()
            total += priority(a)
    return total


def day03_part2(file):
    total = 0
    with open(file) as f:
        while True:
            line = f.readline().strip()
            if not line:
                break
            line2 = f.readline().strip()
            line3 = f.readline().strip()
            s = set(line)
            s = s.intersection(set(line2))
            s = s.intersection(set(line3))
            c = s.pop()
            total += priority(c)
    return total


if __name__ == '__main__':
    # input_filename = 'input.test.txt'
    input_filename = 'input.txt'
    result1 = day03_part1(input_filename)
    print('Result1 = %s' % result1)
    result2 = day03_part2(input_filename)
    print('Result2 = %s' % result2)
