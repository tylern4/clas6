// dMatrix is a double precision matrix class

/*_begin_doc
 *  RCS ID string
 *  $Id: dmatrix.cc,v 1.2 2008/09/05 18:36:59 goetz Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: dmatrix.cc,v 1.2 2008/09/05 18:36:59 goetz Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <iostream>
#include <cstdlib>
#include <cassert>
#include <cstdio>

using namespace std;

// This matrix library uses a substructure, array which allows multiple 
// references to it, and only destructs when there are no references left.
// this allows a matrix to be copied (call by value) without copying the 
// whole array.  It is, however, potentially dangerous and could 
// leak memory if people aren't careful.

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

inline double array::element(int x){
  if(x >= length) {
    // cerr << "out of bounds array error" <<endl;
    throw("out of bounds array error");
  }
  return(a[x]);
}

array::array(int s){
  length=s;
  nref=1;
  a=new double[length];
  for(int i=0;i<length;i++) a[i]=0;
}

array::~array(){
  if(nref>0){
    //cerr << "attempt to destroy array too soon" <<endl;
    throw("attempt to destroy array too soon");
  }
  else
    delete []a;
}  

double& array::operator()(int x){
  if(x >= length) {
    //cerr << "out of bounds array error" <<endl;
    throw("out of bounds array error");
  }
  return(a[x]);
} //do bounds checking

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

dMatrix operator*(const double x,const dMatrix& m1){
  return(m1*x);
}

dMatrix operator*(const dMatrix& m1, const double x){
  dMatrix m(m1.nr(),m1.nc());
  for(int i=0;i<m.nr()*m.nc();i++)
    m(i)=(m1.a)->element(i)*x;
  return(m);
}

dMatrix operator*(const dMatrix& m1, const dMatrix& m2){
  dMatrix m(m1.nr(),m2.nc());
  if(m1.nc() != m2.nr()){
    //cerr << "multiplication of matrices with wrong dimensions" << endl;
    throw("multiplication of matrices with wrong dimensions");
  }
  else{
    for(int k=0;k<m.nc();k++)
      for(int i=0; i<m.nr();i++){
	m(i,k)=0;
	for(int j=0; j<m1.nc();j++)
	  m(i,k)+=m1.a->element(i*m1.nc()+j)*m2.a->element(j*m2.nc()+k);
      }
  }
  return(m);
}

dMatrix operator-(const dMatrix& m1,const dMatrix& m2){
  dMatrix m(m1.nr(),m1.nc());
  if(m.nr() != m2.nr() || m.nc() != m2.nc()){
    //cerr << "subtraction of matrices with differing dimensions" << endl;
    throw("subtraction of matrices with differing dimensions");
  }
  else{
    for(int i=0;i<m.nr()*m.nc();i++)
      m(i)=(m1.a)->element(i) - (m2.a)->element(i) ;
  }
  return(m);
}

dMatrix operator+(const dMatrix& m1,const dMatrix& m2){
  dMatrix m(m1.nr(),m1.nc());
  if(m.nr() != m2.nr() || m.nc() != m2.nc()){
    //cerr << "addition of matrices with differing dimensions" << endl;
    throw("addition of matrices with differing dimensions");
  }
  else{
    for(int i=0;i<m.nr()*m.nc();i++)
      m(i)=(m1.a)->element(i) + (m2.a)->element(i);
  }
  return(m);
}

dMatrix& dMatrix::operator=(const dMatrix& m){
  char message[100];
  if(nrow ==0 && ncol == 0){
      sprintf(message,"error in dMatrix bounds");  nrow = m.nrow;
    ncol = m.ncol;
    a = new array(ncol*nrow);
  }
  if(nrow != m.nrow || ncol != m.ncol) {
    sprintf(message,"assignment of matrices with differing dimensions(%d,%d)=(%d,%d)",nrow,ncol,m.nrow,m.ncol);
    throw(message);
  }
  else{
    for(int i=0;i<nrow*ncol;i++)
      (*this)(i)=(m.a)->element(i);
  }  
  return(*this);
}

dMatrix::dMatrix(const dMatrix& mat){ //copy constructor
  ncol=mat.ncol;
  nrow=mat.nrow;
  a=mat.a;
  (*a).add_ref();
} 

dMatrix::dMatrix(int row, int col){
  if(row<1 || col<1){
    //cerr << "illegal dMatrix size" <<endl;
    throw("illegal dMatrix size");
  }
  ncol=col;
  nrow=row;
  a = new array(ncol*nrow);
}
dMatrix::dMatrix(){
  ncol=0;
  nrow=0;
  a = NULL;
}

dMatrix::~dMatrix(){
  a->del_ref();
  if(a->ref()==0){
    delete a;
  }
}

double& dMatrix::operator()(int row, int col){
  if(col>=ncol || row>=nrow || col<0 || row<0){
    //cout << "error in dMatrix bounds" << endl;
    throw("error in dMatrix bounds");
  }
  return( (*a)(row*ncol+col) );
}

void print(dMatrix M){
  cout << "{";
  for(int i=0;i<M.nr();i++){
    cout << "\t{";
    for(int j=0;j<M.nc();j++){
      cout << M(i,j);
      if(j!=M.nc()-1)
	cout << ",\t";
    }
    cout << "}" << endl;
  }
  cout << "}" << endl; 
}

dMatrix Trn(dMatrix m){ //transpose of the matrix
  dMatrix temp(m.nc(),m.nr());
  for(int i=0;i<m.nr();i++)
    for(int j=0;j<m.nc();j++)
      temp(j,i)=m(i,j);
  return(temp);
}
















