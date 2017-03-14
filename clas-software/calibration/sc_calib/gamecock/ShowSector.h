#ifndef __SHOWSECTOR_H
#define __SHOWSECTOR_H

#include "ROOT.h"
#include "Inx.h"
#include "JndexBox.h"

class ShowSector: public JMultiple {
  int sector;
  int cursorpos;      /// Cursor Position
  TCanvas* Csec;
public:
  ShowSector (int sector_);
  void SetCursor(Inx index);
  void PointerLeftClick(Inx index);
  void Update(Inx index);
};

#endif
