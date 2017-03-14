#include "JInteractive.h"

using namespace std;

JBox::JBox (double x0, double y0, double x1, double y1, JInteractive* super_):
  TBox(x0,y0,x1,y1), super(super_) {
  SetFillColor(6);
  SetFillStyle(3027);
};

void JBox::ExecuteEvent(Int_t event, Int_t px, Int_t py) {
  switch (event) {
  case kMouseEnter:
    gPad->SetCursor(kCross);
    break;
  case kButton1Down:
    x = gPad->PadtoX(gPad->AbsPixeltoX(px));
    y = gPad->PadtoX(gPad->AbsPixeltoY(py));
    super->PointerClick(x,y);
    break;
  }
}

void JBox::Paint(Option_t* opt) {
  SetX1 (gPad->GetUxmin());
  SetX2 (gPad->GetUxmax());
  SetY1 (gPad->GetUymin());
  SetY2 (gPad->GetUymax());
  TBox::Paint(opt);
}

void JInteractive::DrawBox() {
  if (!box) {
    gPad->Modified();
    gPad->Update();
    box = new JBox(gPad->GetUxmin(), gPad->GetUymin(), 
		   gPad->GetUxmax(), gPad->GetUymax(), this);
  }
  box->Draw();
}

