#include <iostream>
#include <tuple>
#include <sstream>

using namespace std;


/**
 * Given the integers n, k, construct an n by n grid of integers
 * where each row and column has at most k distinct integers.
 * The returned grid must have the maximum number of distinct integers.
 *
 * Return your answer as a int[] with exactly n * n elements.
 * Element i*n+j of your return value denotes the value of the
 * integer in the i-th row and j-th column of the grid.
 *
 * If there are multiple optimal solutions, you may return any of them.
 * The integers in your solution can be arbitrary, as long as they fit
 * into a signed 32-bit integer variable.
 *
 * - n will be between 3 and 50, inclusive
 * - k will be between 1 and n / 2, inclusive
 *
 * Expected Input / Output:
 * 3
 * 1
 * Returns: {0, 0, 0, 0, 0, 0, 0, 0, 0}
 * The returned value shown above represents the following grid:
 * {
 *   0,0,0,
 *   0,0,0,
 *   0,0,0
 * }
 */


// If k == 1, number of distinct elements is 1, so just return a matrix of
// zeroes.
// If k == n / 2, number of distinct elements is max, so generate range upfront.


template<typename T>class DistinctGrid {
public:

  int n;
  int *values;

  DistinctGrid(const int n) {
    this->n      = n;
    this->values = new int[n * n];
  }

  ~DistinctGrid() {
    delete this->values;
  }

  T at(const int i, const int j) {
    return this->values[i * this->n + j];
  }
};


// Example of converting a string to an integer:
int string_to_int(string s) {
  stringstream stream(s);
  int result;

  stream >> result;
  return result;
}

// Get input values from stdin
tuple<int, int>get_input() {
  int n, k;

  cin >> n;
  cin >> k;

  return make_tuple(n, k);
}

void findGrid(DistinctGrid<int>grid) {}

int  main(int argc, char const *argv[]) {
  const tuple<int, int> config = get_input();

  DistinctGrid<int> grid(get<0>(config));


  return 0;
}
