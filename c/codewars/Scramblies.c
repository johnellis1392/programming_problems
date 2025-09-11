#include<limits.h> // For CHAR_MAX and CHAR_MIN
#include<stdio.h>
#include<stdbool.h>
#include<stdlib.h>
#include<string.h>

#define str(x) (x==true?"true":"false")

bool scramble2(const char *str1, const char *str2) {
  int buf[CHAR_MAX - CHAR_MIN + 1] = {0};
  for (; *str1; ++str1) ++buf[*str1 - CHAR_MIN];
  for (; *str2; ++str2)
    if (--buf[*str2 - CHAR_MIN] < 0)
      return false;
  return true;
}

bool scramble(const char *str1, const char *str2) {
  if (str1 == NULL || str2 == NULL) return false;
  const int n = strlen(str1), m = strlen(str2);
  int buf[26];
  int i;
  for (i = 0; i < 26; i++) buf[i] = 0;
  for (i = 0; i < n; i++) buf[str1[i] - 'a']++;
  for (i = 0; i < m; i++) buf[str2[i] - 'a']--;
  for (i = 0; i < 26; i++)
    if (buf[i] < 0) return false;
  return true;
}

void test(const char *str1, const char *str2, const bool exp) {
  const bool actual = scramble(str1, str2);
  if (actual == exp) {
    printf("Success\n");
  } else {
    printf("Failure: %s != %s\n", str(actual), str(exp));
  }
}

int main() {
  test("rkqodlw", "world", true);
  test("cedewaraaossoqqyt", "codewars", true);
  test("katas", "steak", false);
  return 0;
}
