#include "ButtonOkCancel.h"

ButtonOkCancel::ButtonOkCancel(const TGWindow *p,  UInt_t w):
  TGCompositeFrame(p, w, 30, kFixedWidth | kHorizontalFrame | kSunkenFrame | kDoubleBorder),
  witgets(new TList) {
  TGTextButton* bOk;
  TGTextButton* bCc;
  TGLayoutHints* xLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | kLHintsBottom, 20, 20, 10, 10 );
  witgets->Add(xLayout);

  bOk = new TGTextButton(this, "&OK", id_Ok);
  witgets->Add(bOk);

  bCc = new TGTextButton(this, "&Cancel", id_Cancel);
  witgets->Add(bCc);

  TGFrame* fill = new TGFrame(this, w/2, 30, kFixedWidth); 
  witgets->Add(fill);

  bOk ->Associate(p);
  bCc ->Associate(p);
  AddFrame (bOk, xLayout);
  AddFrame (fill,xLayout);
  AddFrame (bCc, xLayout);
  //  bOk ->Resize(250,60);
  //  bCc ->Resize(250,60);
  //  Resize (w,h);
}

ButtonOkCancel::~ButtonOkCancel() {
  witgets->Delete();
  delete witgets;
}

