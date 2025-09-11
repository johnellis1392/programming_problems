#include<iostream>
#include<string>

using namespace std;

typedef struct Test {
  string haystack;
  string needle;
  int exp;
} Test;

const Test tests[] = {
	{"sadbutsad", "sad", 0},
	{"leetcode", "leeto", -1},
	{"a", "a", 0},
	{"ba", "a", 1},
};

int strStr(const string haystack, const string needle) {
  if (needle.size() == 0 || needle.size() > haystack.size())
    return -1;
  const int n = haystack.size(), m = needle.size();
  for (int i = 0; i < n-m+1; i++)
    if (haystack.substr(i, m) == needle)
      return i;
  return -1;
}

// Much easier solution
int _strStr2(const string haystack, const string needle) {
  return haystack.find(needle);
}

int main() {
  cout << "Running..." << endl;
  for (const auto t : tests) {
    const auto res = strStr(t.haystack, t.needle);
    if (res == t.exp) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: " << res << " != " << t.exp << endl;
    }
  }
  return 0;
}