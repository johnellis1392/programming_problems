#include<iostream>
#include<vector>
#include<tuple>
#include<map>


using namespace std;


template <typename V> class Node {
public:
    Node(V value): value(value), adjacent(vector<Node*>()) {}
    ~Node() {
        // delete this->value;
    }

protected:
    V value;
    vector<Node*> adjacent;
};



template <typename V> class Graph {
public:
    Graph(): head(NULL) {}

    Graph(vector< tuple<V, V> > conns) {
        // this->nodes = new vector<Node*>();
        this->nodes = new map<int, Node*>;
        for (vector< tuple<V, V> >::iterator i = conns.start(); i != conns.end(); i++) {
            tuple<V, V> t = i;
            Node *node = new Node<int>(j);
            // Insert stuff into map
        }
        // Do other stuff
    }

    ~Graph() {}

protected:
    Node<V> head;
    // vector<Node*> nodes;
    map<int, Node*> nodes;
};



int main() {
    int q;
    cin >> q;

    cout << "Running..." << endl;
    for(int a0 = 0; a0 < q; a0++){
        int n, m;
        long x, y;
        cin >> n >> m >> x >> y;

        vector< tuple<int, int> > conns;
        for(int a1 = 0; a1 < m; a1++){
            int city_1, city_2;
            cin >> city_1 >> city_2;

            conns.push_back(tuple<int, int>(city_1, city_2));
        }

        // Create graph
        Graph<int> graph(conns);

        // Calculate shortest path
        // ...

        // Finish the thing
    }

    return 0;
}
