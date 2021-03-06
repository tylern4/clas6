// Autogenerated Class (Header File)
// Author : G.Gavalian
// Date   : Sun Apr  1 20:42:16 EDT 2007
//

#ifndef __TCTStringUtils__
#define __TCTStringUtils__
#include <iostream>
#include <TROOT.h>
#include <TVector3.h>
#include <TObject.h>
#include <TString.h>

class TCTStringUtils {
private:
  char  cbuff[128];
  TString s_buff;

public:
TCTStringUtils ();
~TCTStringUtils ();

  TString   PullStr(TString *fstr, const char c = ' ');
  void      PushStr(TString pstr, TString *fstr, const char c = ' ');
  int       GetCount(TString _astr,char __crt);
  TString   GetSubstring(TString  a, int start , int len);
  Int_t     GetNTokens(const char cr = ' ');
  TString   GetToken(const char *str,int idx,const char cr = ' ');
  TString   ChangeExtension(TString path, const char *__ext,const char *__next);

  Int_t     GetParNumber(const char *formula);
  TString   GetParName(const char *formula, int _idx);
  Int_t     GetParSign(const char *formula, int _idx);

};
#endif
