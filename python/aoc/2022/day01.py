def read_data(file):
    data = []
    with open(file, newline='') as f:
        for line in f.readlines():
            data.append(line.strip())
    return data


def partition(data):
    result = []
    for i in data:
        if i == '':
            yield result
            result = []
        else:
            result.append(i)
    yield result


def day01_part1(file):
    data = partition(read_data(file))
    result = max(
        map(lambda x:
            sum(map(int, x)), data))
    return result


def day01_part2(file):
    data = partition(read_data(file))
    result = sorted(
        map(lambda x:
            sum(map(int, x)), data),
        reverse=True)
    return sum(result[:3])


if __name__ == '__main__':
    input_filename = 'input.txt'
    result1 = day01_part1(input_filename)
    result2 = day01_part2(input_filename)
    print('Part 1: %s' % result1)
    print('Part 2: %s' % result2)
