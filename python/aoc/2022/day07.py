import re
from itertools import takewhile


def insert_dir(dirlist, current_dir, new_dir):
    d = dirlist
    for x in current_dir:
        d = d[x]
    d[new_dir] = {}


def insert_file(dirlist, current_dir, filename, filesize):
    d = dirlist
    for x in current_dir:
        d = d[x]
    d[filename] = filesize


def calc_dir_sizes(dirlist):
    results = {}
    def f(d, path=['']):
        s = 0
        for k, v in d.items():
            if isinstance(v, dict):
                vs = f(v, path + [k])
                s += vs
            else:
                s += v
        results['/'.join(path)] = s
        return s
    f(dirlist['/'])
    return results


def calc_dir_sizes2(dirlist):
    results = []
    def f(d):
        s = 0
        for _, v in d.items():
            if isinstance(v, dict):
                vs = f(v)
                # results.append(vs)
                s += vs
            else:
                s += v
        results.append(s)
        return s
    results.append(f(dirlist))
    return results


def day07_part1(filename):
    dirs = {'/': {}}
    with open(filename) as f:
        current_dir = []
        row = f.readline().strip()
        while row:
            if row == '$ cd /':
                current_dir = ['/']
            elif row == '$ cd ..':
                current_dir.pop()
            elif row.startswith('$ cd '):
                d = row[len('$ cd '):].strip()
                insert_dir(dirs, current_dir, d)
                current_dir.append(d)
            elif row == '$ ls':
                filelist = []
                row = f.readline().strip()
                while row and not row.startswith('$'):
                    filelist.append(row)
                    row = f.readline().strip()
                for file in filelist:
                    if file.startswith('dir '):
                        insert_dir(dirs, current_dir, file[len('dir '):])
                    else:
                        size, name = re.match(r'^(\d+) (.*)$', file).groups()
                        insert_file(dirs, current_dir, name, int(size))
                continue
            row = f.readline().strip()

    return sum(filter(lambda x: x <= 100000, calc_dir_sizes2(dirs)))


def day07_part2(filename):
    dirs = {'/': {}}
    with open(filename) as f:
        current_dir = []
        row = f.readline().strip()
        while row:
            if row == '$ cd /':
                current_dir = ['/']
            elif row == '$ cd ..':
                current_dir.pop()
            elif row.startswith('$ cd '):
                d = row[len('$ cd '):].strip()
                insert_dir(dirs, current_dir, d)
                current_dir.append(d)
            elif row == '$ ls':
                filelist = []
                row = f.readline().strip()
                while row and not row.startswith('$'):
                    filelist.append(row)
                    row = f.readline().strip()
                for file in filelist:
                    if file.startswith('dir '):
                        insert_dir(dirs, current_dir, file[len('dir '):])
                    else:
                        size, name = re.match(r'^(\d+) (.*)$', file).groups()
                        insert_file(dirs, current_dir, name, int(size))
                continue
            row = f.readline().strip()

    dir_sizes = calc_dir_sizes2(dirs)
    total_size = max(dir_sizes)
    target_mem_size = 40000000
    results = filter(lambda x: total_size - x <= target_mem_size, dir_sizes)
    return list(sorted(results))[0]


def calc_num_dirs(filename):
    ndirs = 0
    with open(filename) as f:
        for line in f.readlines():
            if line.startswith('dir '):
                ndirs += 1
    return ndirs


if __name__ == '__main__':
    # input_filename = 'input.test.txt'
    input_filename = 'input.txt'
    result1 = day07_part1(input_filename)
    print('Result1: %s' % result1)
    result2 = day07_part2(input_filename)
    print('Result2: %s' % result2)
