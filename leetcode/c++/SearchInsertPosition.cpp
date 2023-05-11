#include<iostream>
#include<vector>

using namespace std;

typedef struct Test {
  const vector<int> nums;
  const int target;
  const int exp;
} Test;

const Test tests[] = {
  {vector<int>{1,3,5,6}, 5, 2},
  {vector<int>{1,3,5,6}, 2, 1},
  {vector<int>{1,3,5,6}, 7, 4},
};

int searchInsert(const vector<int>& nums, const int target) {
  int i = 0, j = nums.size();
  while (i < j) {
    int mid = (i + j) / 2;
    if (nums[mid] == target) {
      return mid;
    } else if (nums[mid] < target) {
      i = mid + 1;
    } else {
      j = mid;
    }
  }
  return i;
}

int main() {
  cout << "Running..." << endl;
  for (const Test& test : tests) {
    const auto res = searchInsert(test.nums, test.target);
    if (res == test.exp) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: "
        << res << " != " << test.exp << endl;
    }
  }
  return 0;
}
