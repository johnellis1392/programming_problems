#include<stack>
#include<string>
#include<iostream>

using namespace std;

typedef struct t0 {
  string input;
  bool output;
} Test;

const Test tests[] = {
  {"()", true},
  {"()[]{}", true},
  {"(]", false},
  {"]", false},
};

bool isValid(const string s) {
  stack<char> _stack;
  for (int i = 0; i < s.size(); i++) {
    if (s[i] == '(' || s[i] == '[' || s[i] == '{') {
      _stack.push(s[i]);
    } else if (_stack.size() == 0) {
      return false;
    } else if (
      (s[i] == ')' && _stack.top() == '(') ||
      (s[i] == ']' && _stack.top() == '[') ||
      (s[i] == '}' && _stack.top() == '{')
    ) {
      _stack.pop();
    } else {
      return false;
    }
  }
  return _stack.size() == 0;
}

int main() {
  cout << "Running..." << endl;
  for (const auto test : tests) {
    const auto res = isValid(test.input);
    if (res == test.output) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: " << res << " != " << test.output << endl;
    }
  }
  return 0;
}