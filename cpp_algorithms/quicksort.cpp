#include<iostream>
#include<string>
#include<sstream>
#include<vector>
#include<tuple>
#include<time.h>
#include<cstdlib>
#include<stdio.h>


using namespace std;

template<typename A> void quicksort(A array[]) {
    // ...
}


// void join(const vector<string>& v, char c, string& s) {
//     s.clear();
//
//     for (vector<string>::const_iterator i = v.begin(); i != v.end(); i++) {
//         s += *i;
//         if (i != v.end() - 1)
//             s += c;
//     }
// }
//
//
// void join(const vector<string>& v, string s, ostringstream& output) {
//     output << v[0];
//     for (vector<string>::const_iterator i = v.begin() + 1; i != v.end(); i++)
//         output << s << v[i];
// }
//
//
// string to_string(const int array[], const int size) {
//     ostringstream result;
//     for (int i = 0; i < size; i++)
//         result << array[i];
//     return result.str();
// }


string join(const int array[], const int size, const string s) {
    ostringstream result;
    if (size <= 0) return result.str();

    result << array[0];
    for (int i = 0; i < size; i++)
        result << s << array[i];

    return result.str();
}


string to_string(const int array[], const int size) {
    ostringstream result;
    result << "[";
    result << join(array, size, ", ");
    result << "]";
    return result.str();
}


// void get_input(vector<int>& v) {
//     int n;
//     cin >> n;
//     for (int i = 0; i < n; i++)
//         cin >> v[i];
// }


// Note: This is not for quicksort, but for
// removing a value or something. Change this
// later.
tuple< vector<int>, vector<int> > gen_random_data() {
    const int n = 20;
    const int min = 5;
    const int max = n - 5;

    // Seed random data
    srand(time(NULL));

    // Generate random value
    const int k = rand() % max + min;

    vector<int> input;
    vector<int> expected;
    for (int i = 0; i < n; i++) {
        input.push_back(i);
        if (i != k) expected.push_back(i);
    }

    tuple< vector<int>, vector<int> > result(input, expected);
    return result;
}


int main() {
    cout << "Running quicksort..." << endl;
    const int size = 20;
    int input[size];
    for (int i = 0; i < size; i++) {
        input[i] = 0;
    }

    quicksort(input);
    string arr_string = to_string(input, size);
    // cout << "Array: " << arr_string << endl;
    cout << "Array: " << arr_string << endl;
    return 0;
}
