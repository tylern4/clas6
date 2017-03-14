#ifndef _SLIDERBOX_H
#define _SLIDERBOX_H

#include <stdlib.h>
#include "ParameterSlider.h"
#include "ButtonOkCancel.h"

const int id_SliderOk = 5274;

using namespace std;

class SliderBox : public TGTransientFrame {
  TList* widgets;
  TGWindow* parent;
  ParameterSlider* psli[6];  
  int npar;
  float par[6];
  bool  modified;
  TF1*  func;
  TPad* pad;
public:
  SliderBox(const TGWindow *p, TGWindow *parent_, UInt_t w,
	    int npar_,  TF1* func_, TPad* pad_);
  ~SliderBox();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t so);
  void CloseWindow();
};

#endif
