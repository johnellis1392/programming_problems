#include<sstream>
#include<string>
#include<vector>
#include<unordered_map>
#include<iostream>

using namespace std;

typedef struct t0 {
  vector<int> nums;
  int target;
  vector<int> output;
} Test;

const Test tests[] = {
  { vector<int>{2, 7, 11, 15}, 9, vector<int>{0, 1}, },
  { vector<int>{3, 2, 4}, 6, vector<int>{1, 2}, },
  { vector<int>{3, 3}, 6, vector<int>{0, 1}, },
};

string to_string(const vector<int>& v) {
  stringstream ss;
  ss << "[";
  for (auto i = v.begin(); i != v.end(); i++) {
    if (i != v.begin())
      ss << ", ";
    ss << *i;
  }
  ss << "]";
  return ss.str();
}

string map_to_string(const unordered_map<int, int>& m) {
  stringstream ss;
  ss << "{";
  for (auto i : m) {
    ss << "(" << i.first << ", " << i.second << ")" << ", ";
  }
  ss << "}";
  return ss.str();
}

vector<int> twoSum(const vector<int>& nums, const int target) {
  unordered_map<int, int> m;
  for (int i = 0; i < nums.size(); i++) {
    if (m.count(nums[i])) {
      return {m[nums[i]], i};
    } else {
      m[target - nums[i]] = i;
    }
  }
  return {};
}

int main() {
  cout << "Running..." << endl;
  for (auto test : tests) {
    const auto actual = twoSum(test.nums, test.target);
    if (actual == test.output) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: " << to_string(actual) 
           << " !== "     << to_string(test.output) << endl;
    }
  }
  return 0;
}