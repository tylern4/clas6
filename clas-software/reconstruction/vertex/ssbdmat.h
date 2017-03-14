#include <iostream>
#include <cstdlib>
#include <cassert>

using namespace std;

class ssbdmat{ //same size block diagonal dMatrix
  dMatrix *M;
  int ncol;
  int nrow;
  int nblock;
public:
  ssbdmat(int nr, int nc,int nb);
  ssbdmat(const ssbdmat& mat); //copy constructor
  ~ssbdmat();

  int nr() const {return(nrow);};
  int nc() const {return(ncol);};
  int nb() const {return(nblock);};

  double operator()(int row, int col);//returns VALUE at row,col of ssbdmatrix
  double& operator()(int row, int col,int block);//returns by ref row,col from block
  ssbdmat& operator=(const ssbdmat& m);
  dMatrix& operator[](int i);//returns ith dMatrix

  friend ssbdmat operator+(const ssbdmat& m1,const ssbdmat& m2);
  friend ssbdmat operator-(const ssbdmat& m1,const ssbdmat& m2);
  friend ssbdmat operator*(const ssbdmat& m1, const ssbdmat& m2);
  friend ssbdmat operator*(const ssbdmat& m1, const double x);
  friend dMatrix operator*(const ssbdmat& m1, dMatrix m2);  
  friend dMatrix operator*(dMatrix m1,const ssbdmat& m2);  
};


dMatrix operator*(dMatrix m1,const ssbdmat& m2);
dMatrix operator*(const ssbdmat& m1, dMatrix m2);

ssbdmat operator*(const double x,const ssbdmat& m1);
ssbdmat operator*(const ssbdmat& m1, const double x);
ssbdmat operator*(const ssbdmat& m1, const ssbdmat& m2);
ssbdmat operator-(const ssbdmat& m1,const ssbdmat& m2);
ssbdmat operator+(const ssbdmat& m1,const ssbdmat& m2);

void print(ssbdmat mat);
ssbdmat Trn(ssbdmat m); //transpose of the ssbdmat

















