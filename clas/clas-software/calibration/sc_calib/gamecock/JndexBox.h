#ifndef __JNDEX_BOX_H
#define __JNDEX_BOX_H

#include "ROOT.h"
#include "Inx.h"

class JndexBox;

class JMultiple {
  int n;
  JndexBox* box [N_CHANNEL];
protected:
  void SetNormal(int index);
  void SetHighlight(int index);
public:
  JMultiple(int n_): n(n_) {}
  void CreateBox(Inx index);
  virtual void PointerLeftClick(Inx index) {}
};

class JndexBox: public TBox {
  Inx index;
  JMultiple* super;
public:
  JndexBox (double x0, double y0, double x1, double y1, 
	    Inx& index_, JMultiple* super_);
  void ExecuteEvent(Int_t event, Int_t px, Int_t py);  
  void Paint(Option_t* opt);
};



#endif
