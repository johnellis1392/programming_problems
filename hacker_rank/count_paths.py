
def dump_cache(cache, x1, y1, x2, y2):
    # matrix = [[cache[i, j] if (i, j) in cache else 0 for j in range(x1, x2)] for i in range(y1, y2)]
    # for i in matrix:
    #     print(str(i))
    # print(str(cache))

    matrix = [[0 for j in range(x1, x2 + 1)] for i in range(y1, y2 + 1)]
    for i in range(x1, x2 + 1):
        for j in range(y1, y2 + 1):
            if (i, j) in cache:
                matrix[i][j] = cache[i, j]
    for i in matrix:
        print(str(i))
    print()


def count_paths(x1, y1, x2, y2):
    cache = {}

    def helper():
        cache[x2, y2] = 0

        # Fill in bottom border with default number of directions (1).
        # Can only move right.
        for i in range(0, x2):
            cache[i, y2] = 1

        # Fill in right border with default number of directions (1).
        # Can only move down.
        for i in range(0, y2):
            cache[x2, i] = 1

        dump_cache(cache, x1, y1, x2, y2)
        for i in range(x2 - 1, -1, -1):
            for j in range(y2 - 1, -1, -1):
                cache[i, j] = cache[i + 1, j] + cache[i, j + 1]
                dump_cache(cache, x1, y1, x2, y2)

        # Return solution
        return cache[0, 0]

    return helper()

if __name__ == '__main__':
    # print(count_paths(0, 0, 1, 1))
    print(count_paths(0, 0, 3, 3))
    # print(count_paths(0, 0, 10, 10))
    # print(count_paths(2, 3, 7, 6))

