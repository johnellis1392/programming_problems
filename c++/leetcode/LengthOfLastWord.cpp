#include<iostream>
#include<sstream>
#include<string>
#include<ranges>
#include<string_view>

using namespace std;

typedef struct Test {
  string s;
  int o;
} Test;

const Test tests[] = {
  {"Hello World", 5},
  {"   fly me   to   the moon  ", 4},
  {"luffy is still joyboy", 6},
};

// vector<string> split(string s) {
//   vector<string> v;
//   auto ssplit = s
//     | ranges::views::split(' ')
//     | ranges::views::transform([](auto&& str) {
//         return string_view(&*str.begin(), ranges::distance(str));
//       });
//   for (auto && word : ssplit)
//     v << word;
//   return v;
// }

vector<string> split(const string s) {
  const string delimiter = " ";
  string s_copy = s;
  vector<string> v;
  string token;
  size_t pos = 0;
  while ((pos = s_copy.find(delimiter)) != string::npos) {
    token = s_copy.substr(0, pos);
    if (token.size() > 0)
      v.push_back(token);
    s_copy.erase(0, pos + delimiter.length());
  }
  if (s_copy.size() > 0)
    v.push_back(s_copy);
  return v;
}

string format(const vector<string>& v) {
  stringstream ss;
  ss << "[";
  for (int i = 0; i < v.size(); i++) {
    if (i != 0) ss << ", ";
    ss << v[i];
  }
  ss << "]";
  return ss.str();
}

int lengthOfLastWord2(const string s) {
  vector<string> splits = split(s);
  return splits[splits.size()-1].size();
}

int lengthOfLastWord(const string s) {
  int n = 0;
  for (int i = s.size() - 1; i >= 0; i--)
    if (s[i] == ' ' && n == 0) continue;
    else if (s[i] == ' ' && n != 0) return n;
    else n++;
  return n;
}

int main() {
  cout << "Running..." << endl;
  // const auto s = tests[1].s;
  // cout << format(split(s)) << endl;

  for (const auto t : tests) {
    const auto res = lengthOfLastWord(t.s);
    // const auto res = lengthOfLastWord2(t.s);
    if (res == t.o) {
      cout << "Success" << endl;
    } else {
      cout << "Failure: "
        << res << " != " << t.o << endl;
    }
  }
  return 0;
}
