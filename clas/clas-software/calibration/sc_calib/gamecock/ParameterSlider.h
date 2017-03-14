#ifndef _PARAMETERSLIDER_H
#define _PARAMETERSLIDER_H

#include "ROOT.h"

const int id_sli = 1;
const int id_ent = 2;
const int sliderSteps = 250;

class ParameterSlider: public TGCompositeFrame {
  const TGWindow* parent;
  bool mouseHold;
  TGLabel* lab;
  TGHSlider* sli;
  TGNumberEntry* ent;
  int ipar;
  double value;
  double vmin;
  double vmax;
public:
  ParameterSlider(const TGWindow *p, UInt_t w, UInt_t h,
		  int ipar_, double value_, double vmin_, double vmax_);
  ~ParameterSlider();
  int    SetPosition();
  double GetPosition();
  double GetValue() { return value; }
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t so);
};

#endif
