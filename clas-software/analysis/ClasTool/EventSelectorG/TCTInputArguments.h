// Autogenerated Class (Header File)
// Author : G.Gavalian
// Date   : Tue Dec 18 14:35:05 EST 2007
//

#ifndef __TCTInputArguments__
#define __TCTInputArguments__
#include <iostream>
#include <TROOT.h>
#include <TVector3.h>
#include <TObject.h>
#include <TOrdCollection.h>
#include <TObjString.h>

class TCTInputArguments : public TObject {
private:
  TOrdCollection  *fArgString;
public:
TCTInputArguments ();
~TCTInputArguments ();

 void      AddString(const char *__str);
 TString   GetString();
 TString   GetOption(const char *__opt);
 Int_t     GetNArgs();
 TString   GetArg(int _idx);
 Int_t     FindObject(const char *__str);

ClassDef(TCTInputArguments,1)

};
#endif