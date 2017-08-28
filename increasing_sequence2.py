import unittest


def dump_seqs(sequences):
    print("Sequences:")
    for i in sequences:
        print(str(i))
    print()


def increasing_sequence(input_array):
    if not input_array: return []

    max_seq = [input_array[0]]
    sequences = { tuple(max_seq): 1 }

    for i in range(1, len(input_array)):
        # for j in range(len(sequences) - 1, -1, -1):
        for k in reversed(list(sequences.keys())):
            # seq = sequences[i]
            seq = list(k)

            if seq[-1] < input_array[i]:
                seq.append(input_array[i])
                if tuple(seq) not in sequences:
                    sequences[tuple(seq)] = len(seq)

                if len(seq) > len(max_seq):
                    max_seq = seq

                break
            else:
                if tuple([input_array[i]]) not in sequences:
                    sequences[tuple([input_array[i]])] = 1

        # dump_seqs(sequences)

    return max_seq


class TestIncreasingSequence(unittest.TestCase):
    def test1(self):
        input_data = [1, 6, 7, 4, 6, 1, 3, 4, 6, 19, 12, 14, 35, 66]
        expected = [1, 3, 4, 6, 12, 14, 35, 66]
        actual = increasing_sequence(input_data)
        self.assertEqual(expected, actual)


if __name__ == '__main__':
    unittest.main()

