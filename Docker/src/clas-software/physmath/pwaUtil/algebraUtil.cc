#include <iostream>
#include <fstream>

using namespace std;

#include <algebra.h>

aNumbers::aNumbers()
{
  this->_n = 0;
}

aNumbers::aNumbers(char *file) 
{
  this->set(file);
}

aNumbers::aNumbers(int i)
{
  this->_n = i;
  this->_i = new int [this->_n];
}
 
void aNumbers::display()
{
  int i;
  complex<double> x;
  for (i = 0; i < this->_n; ++i) {
    x = this->_i[i];
    cout.write( (char *) &x,sizeof(complex<double>)); 
    // cout << x << endl;
  }
}

                
 void aNumbers::set(char *file)
{
  ifstream in(file,ios::in);
  int x;
  int ipos = 0;
  this->_n = 0;
  while (in >> x) {
    this->_n++;
  }

  this->_i = new int [this->_n];
  in.close();
  in.open(file,ios::in);
  while (in >> x) {
    this->_i[ipos++] = x;
  }
}

void aNumbers::setElem(int i,int j)
{
  this->_i[i] = j;
}

aNumbers aNumbers::operator+(const aNumbers& N) const 
{
  
  if (this->_n == N._n) {
    aNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_i[i] + N._i[i]);
    }
    return(ret);
  }
  else
    cerr << "error in + operation " << endl;

}
		  
        
  
aNumbers aNumbers::operator-(const aNumbers& N) const 
{
  
  if (this->_n == N._n) {
    aNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_i[i] - N._i[i]);
    }
    return(ret);
  }
  else
    cerr << "error in - operation " << endl;

}
		  
        
cNumbers::cNumbers()
{
  this->_n = 0;
}

cNumbers::cNumbers(char *file) 
{
  this->set(file);
}

cNumbers::cNumbers(int i)
{
  this->_n = i;
  this->_i = new complex<double> [this->_n];
  this->_m = new double [this->_n];
  this->_t = new double [this->_n];
}

void cNumbers::display()
{
  int i;
  complex<double> x;
  for (i = 0; i < this->_n; ++i) {
    x = this->_i[i];
    cout.write( (char *) &x,sizeof(complex<double>));
    //   cout << x << endl;
  }
}

                
 void cNumbers::set(char *file)
{
  ifstream in(file,ios::in);
  double m,t = 0.0;
  complex<double> x;
  int ipos = 0;
  this->_n = 0;
  while (in.read(reinterpret_cast<char *> (&x),sizeof(complex<double>))) {
    this->_n++;
  }
  this->_m = new double [this->_n];
  this->_t = new double [this->_n];
  this->_i = new complex<double> [this->_n];
  in.close();
  in.open(file,ios::in);
  while ( in.read(reinterpret_cast<char *> (&x),sizeof(complex<double>))) {
    this->_i[ipos++] = x;
  }
}

void cNumbers::setElem(int i,double m,double t,complex<double> j)
{
  this->_m[i] = m;
  this->_t[i] = t;
  this->_i[i] = j;
}

cNumbers cNumbers::operator+(const cNumbers& N) const 
{
  
  if (this->_n == N._n) {
    cNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_m[i],this->_t[i],this->_i[i] + N._i[i]);
    }
    return(ret);
  }
  else
    cerr << "error in + operation " << endl;

}
		  
        
  
cNumbers cNumbers::operator-(const cNumbers& N) const 
{
  
  if (this->_n == N._n) {
    cNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_m[i],this->_t[i],this->_i[i] - N._i[i]);
    }
    return(ret);
  }
  else
    cerr << "error in - operation " << endl;

}
		  
cNumbers cNumbers::operator+(complex<double> x) const 
{
  

    cNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_m[i],this->_t[i],this->_i[i] + x);
    }
    return(ret);


}
		  
        
  
cNumbers cNumbers::operator-(complex<double> x) const 
{
  
    cNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_m[i],this->_t[i],this->_i[i] - x);
    }
    return(ret);

}
		  
cNumbers cNumbers::operator*(complex<double> x) const 
{
  
    cNumbers ret(this->_n);
    for (int i = 0; i < this->_n; ++i) {
      ret.setElem(i,this->_m[i],this->_t[i],this->_i[i] * x);
    }
    return(ret);

}
		  
                 
  
                
      
  
