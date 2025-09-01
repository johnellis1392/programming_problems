import re


def day05_part1(filename):
    header_strings, body_strings = [], []
    body_strings = []

    # Read Lines
    with open(filename) as f:
        a = header_strings
        for row in f.readlines():
            if not row.strip():
                a = body_strings
                continue
            a.append(row[:-1])

    # Parse lines in header and body
    stacks = [[] for _ in header_strings[-1].split()]
    header_strings = header_strings[:-1]

    # Assemble stacks
    for x in reversed(header_strings):
        m = re.findall(r'(\s{3}|\[\w\])\s?', x)
        for i in range(1, len(stacks)+1):
            g = m[i-1].strip()
            if g:
                stacks[i-1].append(g[1])

    for s in body_strings:
        m = re.search(r'move (\d+) from (\d+) to (\d+)', s.strip())
        [n, fc, tc] = map(int, m.groups())
        for _ in range(n):
            stacks[tc-1].append(stacks[fc-1].pop())

    return ''.join(x.pop() for x in stacks if len(x) > 0)


def day05_part2(filename):
    header_strings, body_strings = [], []
    body_strings = []

    # Read Lines
    with open(filename) as f:
        a = header_strings
        for row in f.readlines():
            if not row.strip():
                a = body_strings
                continue
            a.append(row[:-1])

    # Parse lines in header and body
    stacks = [[] for _ in header_strings[-1].split()]
    header_strings = header_strings[:-1]

    # Assemble stacks
    for x in reversed(header_strings):
        m = re.findall(r'(\s{3}|\[\w\])\s?', x)
        for i in range(1, len(stacks)+1):
            g = m[i-1].strip()
            if g:
                stacks[i-1].append(g[1])

    for s in body_strings:
        m = re.search(r'move (\d+) from (\d+) to (\d+)', s.strip())
        [n, fc, tc] = map(int, m.groups())
        crates = stacks[fc-1][-n:]
        stacks[fc-1] = stacks[fc-1][:-n]
        stacks[tc-1] = stacks[tc-1] + crates

    return ''.join(x.pop() for x in stacks if len(x) > 0)


if __name__ == '__main__':
    input_filename = 'input.txt'
    # input_filename = 'input.test.txt'
    result1 = day05_part1(input_filename)
    print('Result1 = %s' % result1)
    result2 = day05_part2(input_filename)
    print('Result2 = %s' % result2)
