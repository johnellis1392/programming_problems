#include<string>
#include<iostream>

using namespace std;

typedef struct t0 {
  int in;
  bool out;
} test;

const test tests[] = {
	{5, true},
	{121, true},
	{-121, false},
	{10, false},
};

bool isPalindrome(const int x) {
  if (x < 0) {
    return false;
  }
  const string s = to_string(x);
  for (int i = 0; i < s.size()/2; i++) {
    if (s[i] != s[s.size() - i - 1]) {
      return false;
    }
  }
  return true;
}

int main() {
  cout << "Running..." << endl;
  for (const auto t : tests) {
    const auto actual = isPalindrome(t.in);
    if (actual == t.out) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: " << actual << " != " << t.out << endl;
    }
  }
  return 0;
}