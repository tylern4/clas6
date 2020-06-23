#include "jglobal.h"
#include "SingleStripe.h"

void compose_fname(char* s, const char* stub, const int index,
		   int style = 0);

void fitsingle(int index, SingleStripe* sstr);

   //   SingleStripe ss(gClient->GetRoot(), 700, 1100, 33);

/// if divided canvas, get first pad. Get canvas otherwise
TPad* SingleStripe::GetFirstPad() {
  TPad* pad1 = 
    (TPad*) csing->GetCanvas()->GetListOfPrimitives()->FindObject("csin_1");
  if (!pad1) pad1 = csing->GetCanvas();
  return pad1;
}

Bool_t SingleStripe::ProcessMessage(Long_t msg, Long_t parm1, Long_t ibut) {
  switch (parm1) {
    
  case 0: /// set fit range
    SetRangeInteractive();
    break;

  case 1: /// set fit parameter
    new SliderBox(fClient->GetRoot(), this, 450, gCalib->GetNparFit(), 
		    gFitFn[0][index], GetFirstPad());
    break;

  case 2: /// fit again
    fitsingle(index, this); 
    break;

  case 3: /// no good fit 
    {
      int n = gConst[0]->GetNparset();
      new EnterValues(fClient->GetRoot(), index);
      if (n != gConst[0]->GetNparset()) {
	gSurvey->UpdateValues(index, 1);
	gSurvey->Update();
      }
    }
    break;
  case 4:
    CloseWindow();
    break;

  case id_SliderOk:
    parset = true;
    break;
    
  default:
    cout << "SingleStripe unknown message: " << std::hex << msg 
       << "\t" << parm1 << "\t" << ibut << std::dec << endl;
    break;
  }

  return kTRUE;
}

SingleStripe::~SingleStripe() {
  csing->GetCanvas()->Clear();
  widgets->Delete();
  delete widgets;
  gStripe = NULL;
}

SingleStripe::SingleStripe(const TGWindow *p, UInt_t w, UInt_t h,
			   int index_) : 
  TGTransientFrame(p, this, w, h), 
  widgets(new TList), index(index_), csing(NULL),
  range0(NULL), range1(NULL), cutg(NULL), parset(false),
  histocolor(34) {
  TGLayoutHints* canvasLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 
		      2, 2, 2, 2 );
  widgets->Add(canvasLayout);

  TGLayoutHints* bframeLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsBottom | kLHintsExpandX, 
		      2, 2, 2, 2 );
  widgets->Add(bframeLayout);

  TGLayoutHints* pictureLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsBottom, 2, 2, 2, 2 );  

  widgets->Add(pictureLayout);

  TGLayoutHints* buttonLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandX, 
		      15, 10, 15, 10 );
  widgets->Add(buttonLayout);
  
 
  csing = new TRootEmbeddedCanvas ("csing", this, w, h/10*9);
  widgets->Add(csing);

  csing->GetCanvas()->cd();
  csing->GetCanvas()->SetName("csin");

  AddFrame (csing, canvasLayout);

  fbutt = new TGCompositeFrame (this, w, h/10, kFixedWidth | 
				kHorizontalFrame | kSunkenFrame | kDoubleBorder);
    
  char* butLabel[] = {"set fit &Range", "set fit &Parameter", 
		       "&Fit again", "&No good fit", "&Close"};
  for (int i=0; i<5; i++) {
    xbut[i] = new TGTextButton (fbutt, butLabel[i], i);
    widgets->Add(xbut[i]);
    xbut[i]->Associate(this);
    fbutt->AddFrame(xbut[i], buttonLayout);
  }

  AddFrame (fbutt, bframeLayout);
 
  fmess =  new TGCompositeFrame (this, w, h/10, kFixedWidth | 
				kHorizontalFrame | kSunkenFrame | kDoubleBorder);

  TGPictureButton* infoIcon = 
    new TGPictureButton (fmess, fClient->GetPicture("mb_asterisk_s.xpm"));
  widgets->Add(infoIcon);

  fmess->AddFrame(infoIcon, pictureLayout);
  TGLabel* infoText = new TGLabel (fmess, ( (gCalib->IsTwoDim() || gCalib->IsGraph()) 
      ? "mouse click to select area, double click to complete (upper pad only)"
      : "mouse click select lower and upper limit (upper pad only)") );
  widgets->Add(infoText);

  widgets->Add(fbutt);
  widgets->Add(fmess);

  fmess->AddFrame(infoText, bframeLayout);
  AddFrame (fmess, bframeLayout);

  SetWinName();
  MapSubwindows();
  Layout();
  MapWindow();
  HideFrame (fmess);
}

bool SingleStripe::IsLimits() {
  if (gCalib->IsTwoDim() || gCalib->IsGraph() ) 
    return (cutg != NULL);
  return (range0 != NULL && range1 != NULL);
}
  
double SingleStripe::GetLowerLimit() {
  if (! range0->GetX1())
    throw "SingleStripe: no lower limit";
  return range0->GetX1(); 
}

double SingleStripe::GetUpperLimit() {
  if (! range0->GetX1())
    throw "SingleStripe: no upper limit";
  return range1->GetX1(); 
}

TCutG* SingleStripe::GetCut() {
  if (! cutg)
    throw "SingleStripe: no gaphical cut";
  return cutg; 
}

void SingleStripe::SetWinName() {
  char wname[128];
  compose_fname (wname, gCalib->GetWindowName(), index, 3);
  SetWindowName (wname);
}

void SingleStripe::Clear(int index_) {
  csing->GetCanvas()->Clear();
  range0 = NULL;
  range1 = NULL;
  cutg   = NULL;
  parset = false;
  index  = index_;
  SetWinName();
}

void SingleStripe::SetRangeInteractive() {
  TMarker* m;

  if (range0) delete range0;
  if (range1) delete range1;
  if (cutg)   delete cutg;

  HideFrame (fbutt);
  ShowFrame (fmess);
  GetFirstPad()->cd();

  if (gCalib->IsTwoDim() || gCalib->IsGraph() ) {
//     int savecol = gPad->GetFrame()->GetFillColor();
//     gPad->GetFrame()->SetFillColor(5);
//     gPad->Modified();
//     gPad->Update();
    cutg = (TCutG*) gPad->WaitPrimitive("CUTG","CutG");
//     gPad->GetFrame()->SetFillColor(savecol);
    cutg->SetLineStyle(1);
    cutg->SetLineColor(4);
    cutg->SetLineWidth(1);
    cutg->Draw();
  }
  else {
    m = (TMarker*) gPad->WaitPrimitive("TMarker","Marker");
    range0 = new VLine(m->GetX());
    delete m;
    range0->SetLineStyle(3);
    range0->Draw();
    gPad->Modified();
    gPad->Update();
    m = (TMarker*) gPad->WaitPrimitive("TMarker","Marker");
    range1 = new VLine(m->GetX());
    delete m;
    range1->SetLineStyle(3);
    range1->Draw();
  }
  HideFrame (fmess);
  ShowFrame (fbutt);
  gPad->Modified();
  gPad->Update();
}

void SingleStripe::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}
