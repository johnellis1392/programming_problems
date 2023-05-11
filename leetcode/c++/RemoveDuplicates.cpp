#include<iostream>
#include<vector>
#include<sstream>
#include<string>

using namespace std;

typedef struct Test {
  vector<int> nums;
  int val;
  int k;
  vector<int> exp;
} Test;

Test tests[] = {
  {vector<int>{3,2,2,3}, 3, 2, vector<int>{2, 2}},
  {vector<int>{0,1,2,2,3,0,4,2}, 2, 5, vector<int>{0,1,4,0,3}},
  // {vector<int>{}, 0, 0, vector<int>{}},
};

int removeElement(vector<int>& nums, const int val) {
  int k = nums.size();
  int i = 0;
  while (i < k) {
    if (nums[i] == val) {
      const auto temp = nums[i];
      nums[i] = nums[k-1];
      nums[k-1] = temp;
      k--;
    } else {
      i++;
    }
  }
  return k;
}

int removeElement2(vector<int>& nums, const int val) {
  nums.erase(remove(nums.begin(), nums.end(), val), nums.end());
  return nums.size();
}

int removeElement3(vector<int>& nums, const int val) {
  int i = 0;
  for (const int n : nums)
    if (n != val)
      nums[i++] = n;
  return i;
}

string f(const vector<int> v) {
  stringstream ss;
  ss << "[";
  for (int i = 0; i < v.size(); i++) {
    if (i != 0) ss << ", ";
    ss << v[i];
  }
  ss << "]";
  return ss.str();
}

bool equals(
  const vector<int>& a,
  const vector<int>& b,
  const int k
) {
  if (a.size() < k || b.size() < k) return false;
  for (int i = 0; i < k; i++)
    if (a[i] != b[i]) return false;
  return true;
}

int main() {
  cout << "Running..." << endl;
  for (auto test : tests) {
    const auto res = removeElement(test.nums, test.val);
    if (res == test.k && test.nums == test.exp) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: "
        << res << " != " << test.k << ", "
        << f(test.nums) << " != " << f(test.exp) << endl;
    }
  }
  return 0;
}
