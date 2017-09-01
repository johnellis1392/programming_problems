#include<iostream>

using namespace std;


class MyClass {
public:
    MyClass() {
        cout << "run constructor MyClass::MyClass()" << endl;
    }

    ~MyClass() {
        cout << "run destructor MyClass::~MyClass()" << endl;
    }

    MyClass(const MyClass &x) {
        cout << "run copy constructor MyClass::MyClass(const MyClass&)" << endl;
    }

    MyClass& operator = (const MyClass &x) {
        cout << "run assignation MyClass::operator=(const MyClass&)" << endl;
        // `this` object is already allocated; this function expects
        // a pointer to this object to be returned. This function will
        // implicitly make the return value `this` if nothing is
        // returned, but doing the explicit return avoids the warning.
        return *this;
    }
};


MyClass my_function() {
    cout << "run my_function()" << endl;
    MyClass a;
    cout << "my_function is going to return a..." << endl;
    return a;
}


int main() {
    MyClass b = my_function();

    MyClass c;
    c = my_function();

    return 0;
}
