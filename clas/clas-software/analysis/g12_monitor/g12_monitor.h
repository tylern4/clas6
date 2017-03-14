#include "TROOT.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TApplication.h"
#include "TGApplication.h"
#include "TStyle.h"
#include "TPaveLabel.h"
#include "TGListBox.h"
#include "TList.h"
#include "TGClient.h"
#include "TGButton.h"
#include "TGFrame.h"
#include "TObjString.h"
#include "TLorentzVector.h"
#include "RQ_OBJECT.h"
#include "TFile.h"
#include "TKey.h"

class TList;
class TGListBox;

class MyMainFrame : public TGMainFrame {

RQ_OBJECT("MyMainFrame")

private:
  TGListBox           *fListBox;

public:
  MyMainFrame(const TGWindow *p, UInt_t w, UInt_t h);
  virtual ~MyMainFrame();
  virtual void CloseWindow();
  virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t);

  void PrintSelected();
  void PrintAll();
  void DrawSelected();
  void DrawSpecific(int which);
  
  ClassDef(MyMainFrame,0);
};
