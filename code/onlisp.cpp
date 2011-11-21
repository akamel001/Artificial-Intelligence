// onlisp.cpp
// KV, Feb 2010; C++ equivalent of file onlisp.lisp

#include <iostream>
#include <vector>
using namespace std;

void print_vector(vector<int>);
void add1a(vector<int>&);
vector<int> add1b(vector<int>);
void add1d(vector<int>);

// cumbersome main ... just to test the functions ...

int main()
{
  vector<int> v1;

  for (int i = 1; i <= 4; i++)
    v1.push_back(i);

  cout << "v1: ";
  print_vector(v1);

  add1a(v1);

  cout << "v1 after add1a: ";
  print_vector(v1);

  vector<int> v2 = add1b(v1);

  cout << "v1 after add1b: ";

  print_vector(v1);
  cout << "v2 returned by add1b: ";
  print_vector(v2);

  cout << "printed by add1d(v1): ";
  add1d(v1);

  cout << endl << endl;
  return 0;
}

// utility;
void print_vector(vector<int> v)
{
  for (int i = 0; i < v.size(); i++)
    cout << v[i] << " ";
  cout << endl;
  return;
}

// pass by reference; will change content passed param v;
void add1a(vector<int>& v)
{
  for (int i = 0; i < v.size(); i++)
    v[i] += 1;
  return;
}

// pass by value; will not change the original v;
vector<int> add1b(vector<int> v)
{
  for (int i = 0; i < v.size(); i++)
    v[i] += 1;
  return v;
}

// does NOT produce a vector with elements of v + 1; 
// (common cs201-level fake);

void add1d(vector<int> v)
{
  for (int i = 0; i < v.size(); i++)
    cout << v[i] + 1 << " ";
  return;
}
  
