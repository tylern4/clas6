// same size block diagonal dMatrix implementation
/*_begin_doc
 *  RCS ID string
 *  $Id: ssbdmat.cc,v 1.2 2008/09/05 18:36:59 goetz Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: ssbdmat.cc,v 1.2 2008/09/05 18:36:59 goetz Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <iostream>
#include <cstdlib>
#include <cassert>
#include <dmatrix.h>

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


dMatrix operator*(dMatrix m1,const ssbdmat& m2){ 
  // multiplies ssbd dMatrix by a real dMatrix and returns a dMatrix, 
  // note that there is no way to go back (yet) from a real
  // dMatrix that satisfies the ssbd requirements into an ssbd form
  dMatrix temp(m1.nr(),m2.nc() * m2.nb());
  if(m2.nr()*m2.nb()!=m1.nc())
    throw("error in dimensions multiplying ssbd dMatrix by dMatrix");
  else
    {
      for(int b=0;b<m2.nb();b++)
	for(int i=0;i<m1.nr();i++)
	  for(int j=0;j<m2.nr();j++)
	    for(int k=0;k<m2.nc();k++){
	      temp(i,k+b*m2.nr())+= m1(i,j+b*m2.nr())*m2.M[b](j,k);
	    }    
    } 
  return(temp);
}

dMatrix operator*(const ssbdmat& m1, dMatrix m2){ 
  // multiplies ssbd dMatrix by a real dMatrix and returns a dMatrix, 
  // note that there is no way to go back (yet) from a real
  // dMatrix that satisfies the ssbd requirements into an ssbd form
  dMatrix temp(m1.nr() * m1.nb() ,m2.nc() );
  if(m1.nc()*m1.nb()!=m2.nr())
    throw("error in dimensions multiplying ssbd dMatrix by dMatrix");
  else{
    for(int b=0;b<m1.nb();b++)
      for(int i=0;i<m1.nr();i++)
	for(int j=0;j<m1.nc();j++)
	  for(int k=0;k<m2.nc();k++){
	    temp(i+m1.nr()*b,k)+= m1.M[b](i,j)*m2(j+m1.nc()*b,k);
	  }    
  } 
  return(temp);
}

ssbdmat operator*(const double x,const ssbdmat& m1){
  return(m1*x);
}

ssbdmat operator*(const ssbdmat& m1, const double x){
  ssbdmat m(m1.nr(),m1.nc(),m1.nb());
  for(int i=0;i<m.nb();i++)
     m.M[i]=m1.M[i]*x;
  return(m);
}

ssbdmat operator*(const ssbdmat& m1, const ssbdmat& m2){
  ssbdmat m(m1.nr(),m2.nc(),m1.nb());
  if(m1.nc() != m2.nr() || m1.nb()!=m2.nb()){
    throw("multiplication of ssbd matrices with wrong dimensions");
  }
  else{
    for(int i=0; i<m.nb();i++){
      m.M[i]=m1.M[i]*m2.M[i];
    }
  }
  return(m);
}

ssbdmat operator-(const ssbdmat& m1,const ssbdmat& m2){
  ssbdmat m(m1.nr(),m1.nc(),m1.nb());
  if(m.nr() != m2.nr() || m.nc() != m2.nc() || m1.nb()!=m2.nb()){
    throw("subtraction of ssbd matrices with differing dimensions");
  }
  else{
    for(int i=0;i<m.nb();i++)
      m.M[i]=m1.M[i]-m2.M[i];
  }
  return(m);
}

ssbdmat operator+(const ssbdmat& m1,const ssbdmat& m2){
  ssbdmat m(m1.nr(),m1.nc(),m1.nb());
  if(m.nr() != m2.nr() || m.nc() != m2.nc() ||m1.nb()!=m2.nb()){
    throw("addition of ssbd matrices with differing dimensions");
  }
  else{
    for(int i=0;i<m.nb();i++)
      m.M[i]=m1.M[i]+m2.M[i];
  }
  return(m);
}

ssbdmat& ssbdmat::operator=(const ssbdmat& m){
  if(nrow != m.nrow || ncol != m.ncol || nblock != m.nblock){
    throw("assignment of ssbd matrices with differing dimensions");
  }
  else{
    for(int i=0;i<nblock;i++)
      M[i]=m.M[i];
  }  
  return(*this);
}

ssbdmat::ssbdmat(const ssbdmat& mat){ //copy constructor
  ncol=mat.ncol;
  nrow=mat.nrow;
  nblock=mat.nblock;
  //less complicated then using reference counting, 
  //but if nblock is large and nrow and ncol are small, this could be a waste
  // probablt not.

  M = new dMatrix[nblock]; 
  for(int i=0;i<nblock;i++)
    M[i]=mat.M[i];
} 

ssbdmat::ssbdmat(int row, int col, int nb){
  if(row<1 || col<1 || nb<1){
    throw("illegal ssbdmat size");
  }
  ncol=col;
  nrow=row;
  nblock=nb;
  M = new dMatrix[nblock];
  for(int i=0;i<nblock;i++){
    dMatrix temp(row,col);
    M[i]=temp;
  }
}

ssbdmat::~ssbdmat(){
  //  for(int i=0;i<nblock;i++){
  //  M[i].~dMatrix();
  //}
  delete []M;
}

double& ssbdmat::operator()(int row, int col,int block){
  return((*this)[block](row,col));
}

double ssbdmat::operator()(int row, int col){
  int block1, block2;
  if(col>=ncol*nblock || row>=nrow*nblock || row<0 ||col<0){
    //cout << "error in ssbdmat bounds" << endl;
    throw("error in ssbdmat bounds");
  }
  block1=row/nrow;
  block2=col/ncol;
  if(block1!=block2)
    return(0);//outside of blocks
  return( M[block1](row,col) );
}

dMatrix& ssbdmat::operator[](int i){//returns ith dMatrix
  return(M[i]);
}

void print(ssbdmat mat){
  cout << "[ nb=" << mat.nb() <<endl;
  for(int i=0;i<mat.nb();i++){
    print(mat[i]);
  }
  cout << "]" << endl; 
}

ssbdmat Trn(ssbdmat m){ //transpose of the ssbdmat
  ssbdmat temp(m.nc(),m.nr(),m.nb());
  for(int i=0;i<m.nb();i++)
      temp[i]=Trn(m[i]);
  return(temp);
}
















