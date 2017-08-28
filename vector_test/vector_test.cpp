#include<vector>
#include<iostream>
#include<memory>


using namespace std;

int main() {
    // vector<int> v;
    cout << "Running..." << endl;

    shared_ptr< vector<int> > v = make_shared< vector<int> >();
    v->push_back(1);
    v->push_back(2);
    v->push_back(3);

    for (vector<int>::iterator i = v->begin(); i != v->end(); i++) {
        cout << "Value: " << *i << endl;
    }

    return 0;
}
