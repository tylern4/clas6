#ifndef __ENTERVALUES_H
#define __ENTERVALUES_H

#include "CaldbBox.h"
#include "ButtonOkCancel.h"
#include "jdefine.h"

const int enterval_w = 550;
const int enterval_h = 350;
const int id_ev      = 170;
const int id_evEnt   = 180;

enum { id_evDefault, id_evManual, id_evCopy, id_evAverage, id_evDatabase };

class EnterValues : public TGTransientFrame {
  TList* widgets;
  int ignoreMan;
  int index;
  int nfield;
  TGNumberEntry*  ent[3];
  TGRadioButton*  rbut[5];
  int Uent(int ient, double value);
  int CopyLast();
  int GetAverage();
  int UseDatabase();
public:
  EnterValues(const TGWindow* p, int index);
  ~EnterValues();
  void CloseWindow();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
  bool SelectionOk();
};

#endif
