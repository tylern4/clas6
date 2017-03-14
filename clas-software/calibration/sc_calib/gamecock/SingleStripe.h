#ifndef _SINGLESTRIPE_H
#define _SINGLESTRIPE_H

#include "ROOT.h"
#include "SliderBox.h"
#include "EnterValues.h"
#include "VLine.h"

class SingleStripe : public TGTransientFrame {
  TList* widgets;
  int index;
  TRootEmbeddedCanvas* csing;
  VLine* range0;
  VLine* range1;
  TCutG* cutg;
  bool parset;
  int  histocolor;
  TGCompositeFrame* fbutt;
  TGCompositeFrame* fmess;
  TGTextButton* xbut[5];
  void SetWinName();
public:
  SingleStripe(const TGWindow *p, UInt_t w, UInt_t h, int index_);
  ~SingleStripe();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t ibut);
  void SetRangeInteractive();
  void Clear(int index);
  bool IsLimits();
  bool IsParSet() { return parset; }
  int  GetIndex() { return index;  }
  double GetLowerLimit();
  double GetUpperLimit();
  TCutG* GetCut();
  TPad*  GetFirstPad();
  TCanvas* GetCanvas() {return csing->GetCanvas(); }
  int GetHistoColor() {return histocolor; }
  void CloseWindow();
};


#endif
