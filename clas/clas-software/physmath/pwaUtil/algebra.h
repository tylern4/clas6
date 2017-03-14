#include <complex>
using namespace std;
typedef enum { Sum, Diff, Mult} op_t;
typedef enum {CNumber, CFile, Cop} data_t;


class aNumbers {
 private:
  int _n;
  int *_i;

 public:
  aNumbers();
  aNumbers(char *file);
  aNumbers(int);
  void set(char *file);
  
  aNumbers(int n,int *i);
  aNumbers operator+(const aNumbers& N) const;
  aNumbers operator-(const aNumbers& N) const;
  void display();
  void setElem(int,int);
};


class cNumbers {
 private:
  int _n;
  double *_m;
  double *_t;
  complex<double> *_i;

 public:
  cNumbers();
  cNumbers(char *file);
  cNumbers(int);
  void set(char *file);
  
  cNumbers(int n,int *i);
  cNumbers operator+(const cNumbers& N) const;
  cNumbers operator-(const cNumbers& N) const; 
  cNumbers operator+(complex<double>) const;
  cNumbers operator-(complex<double>) const;
  cNumbers operator*(complex<double>) const;
  void display();
  void setElem(int,double,double,complex<double>);
};


  
  


