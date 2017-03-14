#ifndef _BUTTONOKCANCEL_H
#define _BUTTONOKCANCEL_H

#include "ROOT.h"

const int id_Ok     = 667;
const int id_Cancel = 666;

class ButtonOkCancel: public TGCompositeFrame {
  TList* witgets;
public:
  ButtonOkCancel(const TGWindow *p,  UInt_t w);
  ~ButtonOkCancel();
  //  bool ProcessMessage(Long_t msg, Long_t parm1, Long_t so);
};

#endif
