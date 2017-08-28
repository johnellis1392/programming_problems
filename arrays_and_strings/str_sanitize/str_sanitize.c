#include<stdio.h>
#include<stdlib.h>
#include<string.h>


char * sanitize(const char * s) {
    int n = strlen(s);
    int num_spaces = 0;
    char * result = NULL;

    for (int i = 0; i < n; i ++) {
        if (s[i] == ' ') {
            num_spaces ++;
        }
    }

    result = malloc(n + sizeof(char) * 2 * num_spaces + 1);
    for (int i = 0, j = 0; i < n; i ++) {
        if (s[i] == ' ') {
            result[j++] = '%';
            result[j++] = '2';
            result[j++] = '0';
        } else {
            result[j++] = s[i];
        }
    }

    return result;
}


void test(const char * input, const char * expected) {
    const int n = strlen(expected);
    const char * actual = sanitize(input);

    if (strncmp(actual, expected, n) != 0) {
        printf("[FAIL] Test failed for input '%s'\n", input);
    } else {
        printf("[SUCCESS] Test passed for input '%s'\n", input);
    }

    printf(" * Expected: '%s'\n", expected);
    printf(" * Actual:   '%s'\n", actual);
    printf("\n");
    return;
}

int main() {

    test("asdf qwer zxcv g", "asdf\%20qwer\%20zxcv\%20g");

    return 0;
}
