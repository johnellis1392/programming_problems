import unittest


def dump_seqs(sequences):
    print("Sequences:")
    for i in sequences:
        print(str(i))
    print()


def increasing_sequence(input_array):
    if not input_array: return []

    max_seq = [input_array[0]]
    sequences = [max_seq]
    dump_seqs(sequences)

    for i in range(1, len(input_array)):
        for j in range(len(sequences) - 1, -1, -1):
            seq = sequences[j]

            if seq[- 1] < input_array[i]:
                sequences.append(seq + [input_array[i]])
                if len(sequences[-1]) > len(max_seq): max_seq = sequences[-1]
                break
            else:
                sequences.append([input_array[i]])

        dump_seqs(sequences)

    return max_seq


# print(increasing_sequence())

class TestIncreasingSequence(unittest.TestCase):
    def test1(self):
        input_data = [1, 6, 7, 4, 6, 1, 3, 4, 6, 19, 12, 14, 35, 66]
        expected = [1, 3, 4, 6, 12, 14, 35, 66]
        actual = increasing_sequence(input_data)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()

