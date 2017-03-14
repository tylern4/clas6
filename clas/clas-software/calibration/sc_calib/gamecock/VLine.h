#ifndef _VLINE_H
#define _VLINE_H

#include "ROOT.h"

class VLine : public TLine {
public:
  VLine(double x0) : 
    TLine(x0, gPad->GetUymin(), x0, gPad->GetUymax()) {}
  void Paint(Option_t* opt) {
    SetY1(gPad->GetUymin());
    SetY2(gPad->GetUymax());
    TLine::Paint(opt);
  }
};

#endif
