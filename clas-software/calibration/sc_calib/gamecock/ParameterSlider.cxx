#include "ParameterSlider.h"

using namespace std;

///-------------- ParameterSlider ----------------------------

ParameterSlider::ParameterSlider(const TGWindow *p,  UInt_t w, UInt_t h,
				 int ipar_, double value_, 
				 double vmin_, double vmax_):
  TGCompositeFrame(p, w, h, kHorizontalFrame | kDoubleBorder),
  parent(p), mouseHold(false),
  ipar(ipar_), value(value_), vmin(vmin_), vmax(vmax_) {

  char clabel[20];
  sprintf (clabel, " Parameter %d ", ipar);
  lab = new TGLabel(this, clabel);

  sli = new TGHSlider (this, sliderSteps,  kSlider2 | kScaleBoth, id_sli);
  SetPosition();

  ent = new TGNumberEntry (this, value, 9, id_ent,  TGNumberFormat::kNESReal  );
  GetPosition();
  ent->SetNumber(value);

  TGLayoutHints* flayout =
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY, 5, 5, 5, 5 );

  AddFrame(lab, flayout);
  AddFrame(sli, flayout);
  AddFrame(ent, flayout);
}

ParameterSlider::~ParameterSlider() {
  delete ent;
  delete sli;
  delete lab;
}

Bool_t ParameterSlider::ProcessMessage(Long_t msg, Long_t parm1, Long_t so) {
  switch (parm1) {
  case id_sli:
    mouseHold = true;
    GetPosition();
    ent->SetNumber(value);
    if (GET_SUBMSG(msg) == kSL_RELEASE) {
      mouseHold = false;
      SendMessage(parent, msg, id_sli, ipar);
    }
    break;
  case id_ent:
    if (! mouseHold) {  // ignore entry events when slider is moving
      value = ent->GetNumber();
      SetPosition();
      mouseHold = false;
      SendMessage(parent, msg, id_sli, ipar);
    }
    break;
  default:
    cout << "Unknown message " << ipar << ": \t"
	 <<std::hex << msg << "\t" << parm1 << "\t" << so << std::dec << endl;
    break;
  } 

  return kTRUE;
}

int    ParameterSlider::SetPosition() {
  int ival = (int) floor((value - vmin) / (vmax - vmin) * sliderSteps);
  if (ival < 0)           ival = 0;
  if (ival > sliderSteps) ival = sliderSteps;
  sli->SetPosition(ival);
  return ival;
}

double ParameterSlider::GetPosition() {
  double ival = sli->GetPosition();
  value = ival * (vmax - vmin) / sliderSteps + vmin;
  return value;
}
///---------- End ParameterSlider ----------------------------

