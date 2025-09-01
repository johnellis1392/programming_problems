# Note:
# For opponent:
# - A = Rock
# - B = Paper
# - C = Scissors
#
# And for Player:
# - X = Rock
# - Y = Paper
# - Z = Scissors

rock = 1
paper = 2
scissors = 3

scores = {
    'X': rock,
    'Y': paper,
    'Z': scissors,
}

win = 6
draw = 3
loss = 0

plays = {
    ('A', 'X'): draw,
    ('A', 'Y'): win,
    ('A', 'Z'): loss,
    ('B', 'X'): loss,
    ('B', 'Y'): draw,
    ('B', 'Z'): win,
    ('C', 'X'): win,
    ('C', 'Y'): loss,
    ('C', 'Z'): draw,
}


scores2 = {
    ('A', 'X'): scissors,
    ('A', 'Y'): rock,
    ('A', 'Z'): paper,
    ('B', 'X'): rock,
    ('B', 'Y'): paper,
    ('B', 'Z'): scissors,
    ('C', 'X'): paper,
    ('C', 'Y'): scissors,
    ('C', 'Z'): rock,
}

plays2 = {
    'X': loss,
    'Y': draw,
    'Z': win,
}


def day02_part1(file):
    s = 0
    with open(file) as f:
        for row in f.readlines():
            [om, pm] = row.strip().split(' ')
            s += plays[(om, pm)] + scores[pm]
    return s


def day02_part2(file):
    s = 0
    with open(file) as f:
        for row in f.readlines():
            [om, pm] = row.strip().split(' ')
            s += scores2[(om, pm)] + plays2[pm]
    return s


if __name__ == '__main__':
    input_filename = 'input.txt'
    # input_filename = 'input.test.txt'
    result1 = day02_part1(input_filename)
    print('(part1) Total Score: %s' % result1)
    result2 = day02_part2(input_filename)
    print('(part2) Total Score: %s' % result2)
