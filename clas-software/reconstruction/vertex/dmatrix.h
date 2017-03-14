#include <iostream>
#include <cstdlib>
#include <cassert>
#include <cstdio>

using namespace std;

// This matrix library uses a substructure, array which allows multiple 
// references to it, and only destructs when there are no references left.
// this allows a matrix to be copied (call by value) without copying the 
// whole array.  It is, however, potentially dangerous.

class array{
  double *a;
  int length;
  int nref;
public:
  array(int s);
  ~array();
  int size() const {return length;};
  int ref() const {return nref;};
  void add_ref() {nref++;};
  void del_ref() {nref--;};
  double element(int x); //return element w/out ability to change
  double& operator()(int x); //do bounds checking
  double& operator[](int x){return(a[x]);}; //assume programmer knows what he is doing
};

class dMatrix {
  array *a;
  int ncol;
  int nrow;
public:
  dMatrix(); // creates a 0 by 0 matrix which later can be 
  //set = to any size matrix, but then is fixed at that size.

  dMatrix(int row, int col);
  dMatrix(const dMatrix& mat); //copy constructor
  ~dMatrix();

  int nr() const {return(nrow);};
  int nc() const {return(ncol);};

  double& operator()(int row, int col);//do bounds checking
  double& operator()(int x){return( (*a)(x) );};//return element x in array *a
  dMatrix& operator=(const dMatrix& m);

  friend dMatrix operator+(const dMatrix& m1,const dMatrix& m2);
  friend dMatrix operator-(const dMatrix& m1,const dMatrix& m2);
  friend dMatrix operator*(const dMatrix& m1, const dMatrix& m2);
  friend dMatrix operator*(const dMatrix& m1, const double x);
};


dMatrix operator*(const double x,const dMatrix& m1);
dMatrix operator*(const dMatrix& m1, const double x);
dMatrix operator*(const dMatrix& m1, const dMatrix& m2);
dMatrix operator-(const dMatrix& m1,const dMatrix& m2);
dMatrix operator+(const dMatrix& m1,const dMatrix& m2);
dMatrix Trn(dMatrix m); //transpose of the matrix

void print(dMatrix M);



















