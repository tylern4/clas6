#include "EnterValues.h"
#include "SConstants.h"
#include "gamecock.h"

extern Calibration* gCalib;
extern SConstants* gConst[5];
extern GoGamecock* gCock;

void compose_fname (char* s, const char* stub, const int index,
		    int style = 0);

using namespace std;

char* rblabel[] = { "use default values", "enter manually", "copy from previous channel",
		    "average equivalent bars", "select from database"};


//--------------- EnterValues --------------------------
Bool_t EnterValues::ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2) {
  bool understood = true;
  if (parm1 >= id_evEnt && parm1 < id_evEnt + nfield) {
    if (ignoreMan) ignoreMan--;
    else
      rbut[id_evManual]->SetState(kButtonDown);
  }
  else {
    switch (parm1) {
    case id_Ok:
      if (SelectionOk()) CloseWindow();
      else new TGMsgBox(fClient->GetRoot(), this, "error", 
			"no method selected", kMBIconStop);
      break;
    case id_Cancel:
      CloseWindow();
      break;
    case (id_ev+id_evDefault):
      for (int i = 0; i<nfield; i++) {
	ignoreMan+= Uent(i, gCalib->GetDefaultPar(i+gCalib->GetSaveOffs()));
      }
      break;
    case (id_ev+id_evManual):
      ignoreMan = 0;
      break;
    case (id_ev+id_evCopy):
      ignoreMan += CopyLast();
      break;
    case (id_ev+id_evAverage):
      ignoreMan += GetAverage();
      break;
    case (id_ev+id_evDatabase):
      ignoreMan += UseDatabase();
      break;
    default:
      understood = false;
      break;
    }
  }
  if (!understood)
    cout << "Unknown EnterValues Message: " 
	 << std::hex << msg << "\t" << parm1 
	 << "\t" << parm2 << std::dec << endl;

  return kTRUE;
}

EnterValues::EnterValues(const TGWindow* p, int index_) :
  TGTransientFrame(p, this, enterval_w, enterval_h), 
  widgets(new TList), ignoreMan(0),
  index(index_), nfield(gCalib->GetNparSave()) {
  
  TGLabel*           lab[3];
  TGHorizontalFrame* ffent[3];

  TGLayoutHints* LXB = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | kLHintsBottom, 2, 2, 2, 2 );
  widgets->Add(LXB);

  TGLayoutHints* LXY = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX |
		      kLHintsTop | kLHintsExpandY, 2, 2, 2, 2 );  
  widgets->Add(LXY);

  TGLayoutHints* LLY = 
    new TGLayoutHints(kLHintsLeft | 
		      kLHintsTop | kLHintsExpandY, 2, 2, 2, 2 );  
  widgets->Add(LLY);

  TGLayoutHints* LRY = 
    new TGLayoutHints(kLHintsRight | 
		      kLHintsTop | kLHintsExpandY, 2, 5, 2, 2 );  
  widgets->Add(LRY);

  TGLayoutHints* LX = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | 
		      kLHintsCenterY, 2, 2, 2, 2 );
  widgets->Add(LX);

  TGHorizontalFrame* frm = new TGHorizontalFrame(this, enterval_w, enterval_h);
  widgets->Add(frm);

  TGVerticalFrame* flab = new TGVerticalFrame(this, enterval_w/4, enterval_h);
  widgets->Add(flab);

  TGVerticalFrame* fent = new TGVerticalFrame(this, enterval_w/4, enterval_h);
  widgets->Add(fent);

  TGVButtonGroup* bg = new TGVButtonGroup(this, "Method to get parameter");
  widgets->Add(bg);
 
 for (int i= id_evDefault; i <= id_evDatabase; i++) {
    rbut[i] = new TGRadioButton(bg, rblabel[i], id_ev+i);
    widgets->Add(rbut[i]);
    rbut[i]->Associate(this);
    bg->AddFrame(rbut[i], LLY);
  }

  for (int i=0; i<nfield; i++) {
    char parlab[40];
    if (gCalib->GetType()==c_gmean) 
      sprintf (parlab, "gmean => param. %d", i+gCalib->GetSaveOffs());
    else
      sprintf (parlab, "%s %s => param. %d", gCalib->GetSubsystem(i), gCalib->GetItem(i), 
	       i+gCalib->GetSaveOffs());
    lab[i] = new TGLabel(flab, parlab);
    widgets->Add(lab[i]);
    flab->AddFrame(lab[i], LRY);
    ffent[i] = new TGHorizontalFrame(fent, enterval_w/4, enterval_h/nfield);
    widgets->Add(ffent[i]);
    ent[i] = new TGNumberEntry(ffent[i], 0., 12, id_evEnt+i, TGNumberFormat::kNESReal  );
    widgets->Add(ent[i]);
    ent[i]->Associate(this);
    ffent[i]->AddFrame(ent[i], LX);
    fent->AddFrame(ffent[i], LRY);
  }

  frm->AddFrame(bg,   LLY);
  frm->AddFrame(fent, LRY);
  frm->AddFrame(flab, LRY);

  AddFrame(frm, LXY);

  ButtonOkCancel* bok;
  AddFrame(bok = new ButtonOkCancel(this, enterval_w), LXB);
  widgets->Add(bok);

  char wname[128];
  compose_fname(wname, "Get Parameter for", index, 3);
  SetWindowName(wname);
  MapSubwindows();
  Layout();
  MapWindow();
  fClient->WaitFor(this);
}

EnterValues::~EnterValues() {
  widgets->Delete();
  delete widgets;
}

void EnterValues::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}

int EnterValues::Uent(int ient, double value) {
  double oldval = ent[ient]->GetNumber();
  if (oldval == value) return 0;
  //  cout << "Update " << ient << " \t " << oldval << " -> " << value << endl;
  ent[ient]->SetNumber(value);
  return 1;
}

int EnterValues::CopyLast() {
  double values[3];
  bool found = false;

  for (int i = index-1; i >= 0; i--) {
    if (gConst[0]->GetChisquare(i)) {
      for (int j=0; j<gCalib->GetNparSave(); j++)
	values[j] = gConst[0]->GetParameter(i, j+gCalib->GetSaveOffs());
      found = true;
      break;
    }
  }
  if (!found) return 0;

  int retval = 0;
  for (int i=0; i<gCalib->GetNparSave(); i++) {
    double oldval = ent[i]->GetNumber();
    if (values[i] != oldval) {
      ent[i]->SetNumber(values[i]);
      retval++;
    }
  }
  return retval;
}

int EnterValues::GetAverage() {
  double values[3] = { 0., 0., 0. };
  int n = 0;
  int istripe = index % N_SECTOR;
  for (int isec = 0; isec < 6; isec++) {
    int i = isec * N_SECTOR + istripe;
    if (i != index && gConst[0]->GetChisquare(i)) {
      for (int j=0; j<gCalib->GetNparSave(); j++)
	values[j] += gConst[0]->GetParameter(i, j+gCalib->GetSaveOffs());
      n++;
    }
  }
  if (!n) return 0;

  int retval = 0;
  for (int i=0; i<gCalib->GetNparSave(); i++) {
    values[i] /= n;
    double oldval = ent[i]->GetNumber();
    if (values[i] != oldval) {
      ent[i]->SetNumber(values[i]);
      retval++;
    }
  }
  return retval;
}

int EnterValues::UseDatabase() {
  SConstants tempConst;
  new CaldbBox(fClient->GetRoot(), &tempConst);
  if (!tempConst.IsParset()) return 0;

  if (gCalib->GetType()==c_gmean) tempConst.Mipadc2Gmean(index);

  int retval = 0;
  for (int i = 0; i<nfield; i++)
    retval+= Uent(i, tempConst.GetParameter(index,i+gCalib->GetSaveOffs()));
  
  return retval;
}

bool EnterValues::SelectionOk() {
  int iSel = -1;
  for (int i=0; i<5; i++) 
    if (rbut[i]->GetState() == kButtonDown)
      iSel = i;

  if (iSel < 0) return false;

  for (int i=0; i<nfield; i++)
    gConst[0]->SetParameter(index, i+gCalib->GetSaveOffs(),
			    ent[i]->GetNumber());  

  char cname [80], cmesg[255];
  compose_fname (cname, "stripe ", index, 1);
  sprintf (cmesg, "%s: set method [%s]", cname, rblabel[iSel]);
  gCock->AddLog(cmesg);

  return true;
}

//--------------- EnterValues --------------------------

