#ifndef __JINTERACTIVE_H
#define __JINTERACTIVE_H

#include <TBox.h>
#include <TPad.h>
#include <TVirtualX.h>
#include <Buttons.h>
#include "jdefine.h"

class JBox;

class JInteractive {
  JBox* box;
public:
  JInteractive(): box(NULL) {};
  void DrawBox();
  virtual void PointerClick(double x, double y) = 0;
};

class JBox: public TBox {
  double x, y;
  JInteractive* super;
public:
  JBox (double x0, double y0, double x1, double y1, JInteractive* super_);
  void ExecuteEvent(Int_t event, Int_t px, Int_t py);  
  void Paint(Option_t* opt);
};

#endif
