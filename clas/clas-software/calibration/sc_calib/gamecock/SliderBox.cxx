#include "jglobal.h"
#include "SliderBox.h"

const int hrow=57;


Bool_t SliderBox::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  switch (parm1) {
  case id_sli:
    if (index < 0 || index >= 6) {
      cout << "SliderBox::ProcessMessage: bad index " << std::hex << msg << " " 
	   << parm1 << " " << index << std::dec << endl;
      throw "SliderBox::ProcessMessage: bad index";
    }
    if (func) func->SetParameter(index, psli[index]->GetValue());
    if (pad) { pad->Modified(); pad->Update(); }
    modified = true;
    break;
  case id_Ok:
    SendMessage(parent, msg, id_SliderOk, index);
    CloseWindow();
    break;
  case id_Cancel:
    if (modified) {
      for (int i=0; i<npar; i++) {
	if (func) func->SetParameter(i, par[i]);
	if (pad) { pad->Modified(); pad->Update(); }
      }
    }
    CloseWindow();
    break;
  default:
    cout << "SliderBox: " << std::hex << msg << "\t" << parm1 << "\t" << index << std::dec << endl;
    break;
  }

  return kTRUE;
}

SliderBox::SliderBox(const TGWindow *p, TGWindow *parent_, UInt_t w,
		     int npar_,  TF1* func_, TPad* pad_) : 
  TGTransientFrame(p, parent_, w, (npar_+3)*hrow), parent(parent_),
  npar(npar_), modified(false), func(func_), pad(pad_) {

  TGLayoutHints* fLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY );

  TRootEmbeddedCanvas* cformula =
    new TRootEmbeddedCanvas("cformula", this, w, 2*hrow);

  cformula->GetCanvas()->cd();
  TLatex* l = new TLatex;
  l->SetTextAlign(22);
  l->SetTextSize(0.35);
  l->DrawLatex(0.5, 0.5, gCalib->GetLatexFunction());
  AddFrame(cformula, fLayout);

  for (int i=0; i<npar; i++) {
    if (func) par[i] = func->GetParameter(i);
    psli[i] = new ParameterSlider (this, w, hrow, i, par[i], 
				   gCalib->GetLowerLimit(i),
				   gCalib->GetUpperLimit(i));
    AddFrame(psli[i], fLayout);
  }

  ButtonOkCancel* fbut = new ButtonOkCancel (this, w);
  AddFrame(fbut, fLayout);
 
  MapSubwindows();
  Layout();
  MapWindow();
  fClient->WaitFor(this);
}

SliderBox::~SliderBox() {
  for (int i=0; i<npar; i++) 
    if (psli[i]) delete psli[i];
}

void SliderBox::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}
