#include<iostream>
#include<sstream>
#include<string>
#include<vector>

using namespace std;

typedef struct Test {
  vector<int> i;
  vector<int> o;
} Test;

const Test tests[] = {
  {{1,2,3}, {1,2,4}},
  {{4,3,2,1}, {4,3,2,2}},
  {{9}, {1,0}},
};

vector<int> plusOne(vector<int>& digits) {
  for (int i = digits.size() - 1; i >= 0; i--) {
    if (digits[i] == 9) {
      digits[i] = 0;
    } else {
      digits[i]++;
      break;
    }
  }
  if (digits[0] != 0)
    return digits;
  digits.insert(digits.begin(), 1);
  return digits;
}

bool equals(const vector<int>& a, const vector<int>& b) {
  if (a.size() != b.size()) return false;
  for (int i = 0; i < a.size(); i++)
    if (a[i] != b[i])
      return false;
  return true;
}

string format(const vector<int>& v) {
  stringstream ss;
  ss << "[";
  for (int i = 0; i < v.size(); i++) {
    if (i != 0) ss << ", ";
    ss << v[i];
  }
  ss << "]";
  return ss.str();
}

int main() {
  cout << "Running..." << endl;
  for (auto t : tests) {
    auto r = plusOne(t.i);
    if (equals(r, t.o)) {
      cout << "Success" << endl;
    } else {
      cout << "Failure " << format(r) << " != " << format(t.o) << endl;
    }
  }
  return 0;
}
