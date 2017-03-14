#include <iostream>
#include <cstdlib>
#include <cstring>

#include <algebra.h>
#include <strings.h>

using namespace std;

typedef complex<double> double_complex;

data_t data(char *);
int isNumber(char *);
int isOp(char *);


char *removeBlanks(char *c)
{
  char *k = new char[strlen(c)];
  int iptr = 0;
  for (int i = 0; *(c + i); ++i) {
    if (*(c + i) != ' ') {
      *(k + iptr++) = *(c + i);
    }
  }
  *(k + iptr) = '\0';
  strcpy(c,k);
  free(k);
  return(c);
}

double_complex atoc(char *c)
{
  float re,im;
  char *word;

  if ( (*c == '(') && (*(c + strlen(c) -1) == ')')) {
    word = strtok((c+1),",");
    re = atof(word);
    word = strtok(NULL,")");
    im = atof(word);
    cout << re << " " << im << " " << endl;
    return(double_complex(re,im));
  }
  else
    return(double_complex(0.0,0.0));
}

int isCNumber(char *c)
{
  float re,im;
  char *word;
  if ( (*c == '(') && (*(c + strlen(c) -1) == ')')) {
     return(1);
  }
  else
    return(0);
}


int isNumber(char *c)
{
  int npt = 0;
  while (*c) {
    switch (*c) {
    case '.':
      if (npt++)
	return(0);
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      break;
    default:
      return(0);
    }
    c++;
  }
  return(1);
}

int isOp(char *c)
{
  if (strlen(c) != 1)
    return(0);
  switch (*c) {
  case '+':
  case '-':
  case '*':
    return(1);
  default:
    return(0);
  }
}



data_t data(char *c)
{

  data_t ret;

  if (isCNumber(c))
    ret = CNumber;
  else if (isOp(c))
    ret = Cop;
  else
    ret = CFile;
  return(ret);
}

main(int argc, char **argv)
{

  char *argptr;
  op_t op;
  cNumbers K[3];
  int ifile = 0;
  data_t d;
  data_t dt[2];
  int iop = 0;
  double_complex x;

  for (int iarg = 1; iarg < argc; ++iarg) {
    argptr = removeBlanks(argv[iarg]);
    cerr << "ARGUMENT " << argptr << endl;
    d = data(argptr);
    switch (d) {
    case Cop:
      switch (*argptr) {
      case '+':
	op = Sum;
	break;
      case '-':
	op = Diff;
	break;
      case '*':
	op = Mult;
	break;
      }
      break;
    case CFile:
      K[ifile].set(argptr);
      // K[ifile].display();
      ifile++;
      dt[iop++] = d;
      break;
    case CNumber:
      x = atoc(argptr);
      dt[iop++] = d;
      break;
    }
  }

  if (dt[0] == CFile && dt[1] == CFile) {
    switch (op) {
    case Sum:
      K[2] = K[0] + K[1];
      break;
    case Diff:
      K[2] = K[0] - K[1];
      break;
    }
  }

  if (dt[0] == CNumber && dt[1] == CFile) {
    switch (op) {
    case Sum:
      K[2] = K[0] + x;
      break;
    case Diff:
      K[2] = K[0] - x;
      break;
    case Mult:
      K[2] = K[0] * x;
    }
  }

  K[2].display();

}

