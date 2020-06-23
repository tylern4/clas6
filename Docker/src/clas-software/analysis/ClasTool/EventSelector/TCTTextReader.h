// Autogenerated Class (Header File)
// Author : G.Gavalian
// Date   : Tue Jul 10 15:25:05 EDT 2007
//

#ifndef __TCTTextReader__
#define __TCTTextReader__
#include <iostream>
#include <TROOT.h>
#include <TVector3.h>
#include <TObject.h>
#include <fstream>
#include <sstream>

using namespace std;

class TCTTextReader : public TObject {

private:
  ifstream  ifs;
  string    str_line;
public:

TCTTextReader ();
~TCTTextReader ();

 void     Open(const char *fname);
 void     Close();
 int      Next();
 string   GetString(){return str_line;}
 double    GetNumber(int n);
 TVector3  GetVect(int _start);


 ClassDef(TCTTextReader,1)

};
#endif