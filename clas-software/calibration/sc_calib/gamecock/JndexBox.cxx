#include "JndexBox.h"

void JMultiple::CreateBox(Inx index) {
  double x0 = gPad->GetUxmin();
  double x1 = gPad->GetUxmax();
  double y0 = gPad->GetUymin();
  double y1 = gPad->GetUymax();

  int i = index.GetIndex();
  box[i] = new JndexBox( x0, y0, x1, y1, index, this);
  box[i]->Draw();
}

JndexBox::JndexBox (double x0, double y0, double x1, double y1, 
		    Inx& index_, JMultiple* super_):
  TBox(x0,y0,x1,y1), index(index_), super(super_) {
  SetFillColor(6);
  SetFillStyle(3027);
}

void JndexBox::Paint(Option_t* opt) {
  SetX1 (gPad->GetUxmin());
  SetX2 (gPad->GetUxmax());
  SetY1 (gPad->GetUymin());
  SetY2 (gPad->GetUymax());
  TBox::Paint(opt);
}

void JMultiple::SetNormal(int index) {
  box[index] -> SetFillStyle(3027);
}

void JMultiple::SetHighlight(int index) {
  box[index] -> SetFillStyle(3003);
}

void JndexBox::ExecuteEvent(Int_t event, Int_t px, Int_t py) {
  switch (event) {
  case kMouseEnter:
    gPad->SetCursor(kCross);
    break;
  case kButton1Down:
    super->PointerLeftClick(index);
    break;
  }
}

