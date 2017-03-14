#ifndef __JSURVEY_H
#define __JSURVEY_H
#include <math.h>
#include <fstream>
#include "ROOT.h"
#include "SConstants.h"
#include "JInteractive.h"

class JSurvey : public TGMainFrame {
  int n;
  int iscon;
  int currentSector;
  int currentIndex;
  class Histo: public JInteractive {
    int isubpad;
    TH1F* histo;
    TH1F* hdone;
    TH1F* hcomp[4];
    JSurvey* super;
  public:
    Histo(int isubpad_, JSurvey* super_);
    void PointerClick(double x, double y);
    void AddHcomp(TH1F* hcomp_);
    void DrawPad();
    void SetValue(int index, double value, int ihist=0);
    void Write();
    void Dump();
    void ShowSector(int sec);
    double GetValue(int index);
    TAxis* GetXaxis() {return histo->GetXaxis();}
    void SetColor(int ihist, int color);
  };
  Histo* hsurvey[4];
  TLine* cursor[4];
  TRootEmbeddedCanvas* jpad[7];
public:
  JSurvey(const TGWindow *p, UInt_t w, UInt_t h);
  ~JSurvey();
  void CloseWindow();
  void WriteValues();
  void WriteHistogram();
  void Update();
  void ShowSector(int sec);
  void UpdateValues(int id, int ihist=0);
  void SelectPad(int ipad){ jpad[currentSector]->GetCanvas()->cd(ipad); }
  void SetCursor(int index);
  bool GetCaldbConst();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t index);
  bool MenuSelection(Long_t menu_item);
};


#endif
