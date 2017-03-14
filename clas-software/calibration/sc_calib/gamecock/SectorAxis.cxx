#include "SectorAxis.h"

using namespace std;

void SectorAxis::Paint(Option_t* opt) {
  if (ax) {
    int i0 = ax->GetFirst();
    int i1 = ax->GetLast();
    if (i0) {
      SetWmin(ax->GetBinLowEdge(i0)-sector*N_SECTOR+1);
      SetWmax(ax->GetBinUpEdge(i1)-sector*N_SECTOR+1);
    }
    else {
      SetWmin(0.5);
      SetWmax(N_SECTOR + 0.5);
    }
  }
  SetX1(gPad->GetUxmin());
  SetX2(gPad->GetUxmax());
  SetY1(gPad->GetUymax());
  SetY2(gPad->GetUymax());
  TGaxis::Paint(opt);
}
