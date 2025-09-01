#include<iostream>
#include<fstream>
#include<sstream>
#include<vector>
#include<string>

using namespace std;
const int 
  win = 6,
  loss = 0,
  draw = 3,
  rock = 1,
  paper = 2,
  scissors = 3;

int calc_score1(const string om, const string pm) {
  if (om == "A" && pm == "X") {
    return draw + rock;
  } else if (om == "A" && pm == "Y") {
    return win + paper;
  } else if (om == "A" && pm == "Z") {
    return loss + scissors;
  } else if (om == "B" && pm == "X") {
    return loss + rock;
  } else if (om == "B" && pm == "Y") {
    return draw + paper;
  } else if (om == "B" && pm == "Z") {
    return win + scissors;
  } else if (om == "C" && pm == "X") {
    return win + rock;
  } else if (om == "C" && pm == "Y") {
    return loss + rock;
  } else if (om == "C" && pm == "Z") {
    return draw + scissors;
  }
  // Base case: should never be hit
  return 0;
}

int calc_score2(const string om, const string pm) {
  if (om == "A" && pm == "X") {
    return loss + scissors;
  } else if (om == "A" && pm == "Y") {
    return draw + rock;
  } else if (om == "A" && pm == "Z") {
    return win + paper;
  } else if (om == "B" && pm == "X") {
    return loss + rock;
  } else if (om == "B" && pm == "Y") {
    return draw + paper;
  } else if (om == "B" && pm == "Z") {
    return win + scissors;
  } else if (om == "C" && pm == "X") {
    return loss + paper;
  } else if (om == "C" && pm == "Y") {
    return draw + scissors;
  } else if (om == "C" && pm == "Z") {
    return win + rock;
  }
  // Base case: should never be hit
  return 0;
}

int main() {
  cout << "Starting..." << endl;
  // ifstream file("input.test.txt");
  ifstream file("input.txt");
  string line;
  int score1 = 0, score2 = 0;

  while (getline(file, line)) {
    istringstream iss(line);
    string a, b;
    if (!(iss >> a >> b)) break;
    score1 += calc_score1(a, b);
    score2 += calc_score2(a, b);
  }

  printf("Result1: %d\n", score1);
  printf("Result2: %d\n", score2);

  return 0;
}
