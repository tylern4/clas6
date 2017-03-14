#ifndef __SECTORAXIS_H
#define __SECTORAXIS_H

#include "ROOT.h"
#include "jdefine.h"

class SectorAxis : public TGaxis {
  int sector;
  TAxis* ax;
public:
  SectorAxis(int sector_, TAxis* ax_): 
    TGaxis(gPad->GetUxmin(), gPad->GetUymax(), 
	   gPad->GetUxmax(), gPad->GetUymax(), 
	   0.5, N_SECTOR+0.5, 510, "-"),
    sector(sector_-1), ax(ax_) {}
  void Paint(Option_t* opt);
};

#endif
