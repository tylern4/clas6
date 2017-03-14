#include "CaldbBox.h"

using namespace std;

extern Calibration* gCalib;
extern int SC_Version_Flag;
static int runnoDefault=0;
string CaldbBox::sRunIndex = "RunIndex";

int caldbb_height() {
  return (gCalib->GetNparLoad()+7) * 40;
}

///------------------ CaldbTabSel -------------------------------
CaldbTabSel::CaldbTabSel (const char* sys, const char* subsys, const char* it ) :
  selectionDone(false), itemValueId(0), runno(0), 
  system(sys), subsystem(subsys), item(it) {
}

void CaldbTabSel::SetSettings(const CaldbBox* cbox) {
  runno = (int) cbox->runno->GetNumber();

  hostname = string(cbox->ent[id_Hostname]->GetText());
  user     = string(cbox->ent[id_User    ]->GetText());
  passwd   = string(cbox->ent[id_Passwd  ]->GetText());
  //  runindex = string(cbox->ent[id_RunIndex]->GetText());
  runindex = "calib_user.RunIndexg9";
}

bool CaldbTabSel::CommonSettings(const CaldbBox* cbox) {
  SetSettings(cbox);
  CaldbServer caldb(hostname, user, passwd);
  if(runno <= 55356){
    SC_Version_Flag = 1;
    system = "SC_CALIBRATIONS";
  }else{
    SC_Version_Flag = 2;
    system = "SC_CALIBRATIONS_V2";
  }
  int itemId = caldb.GetItemId(system, subsystem, item);
  TSQLResult* res = caldb.GetHistory(itemId, runindex, runno);
  if (res->GetRowCount() < 1) 
    throw "CaldbTabSel: no values found for selected RunIndex and run number";
  TSQLRow* row = res->Next();
  itemValueId = atoi(row->GetField(4));

  return true;
}

TGString* CaldbTabSel::GetItem() {
  ostringstream retitem;
  retitem << subsystem << " " << item << ":";
  return new TGString(retitem.str().c_str());
}

TGString* CaldbTabSel::GetLabel() {
  ostringstream retlab;
  if (selectionDone)
    retlab << "<" << itemValueId << "> from " << date << " by "
	<< author << " \"" << comment << "\"";
  else {
    retlab << "get most recent table";
    if (runno) retlab << " for run " << runno;
    retlab << " using selected RunIndex";
  }
  return new TGString(retlab.str().c_str());
}

bool CaldbTabSel::SelectTable(CaldbBox* p) {
  CaldbServer caldb(hostname, user, passwd);
  if(runno <= 55356){
    SC_Version_Flag = 1;
    system = "SC_CALIBRATIONS";
  }else{
    SC_Version_Flag = 2;
    system = "SC_CALIBRATIONS_V2";
  }
  int itemId = caldb.GetItemId(system, subsystem, item);
  TSQLResult* res = caldb.GetHistory(itemId, runindex, runno);
  bool saveSelectionDone = selectionDone;
  selectionDone = false;
  new CaldbHistory(p->fClient->GetRoot(),p, this, res);
  if (selectionDone) p->Label4Table();
  else selectionDone = saveSelectionDone;
  return true;
}

///------------------ CaldbTabSel -------------------------------

///------------------ CaldbBox ----------------------------------
Bool_t CaldbBox::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  bool understood = true;

  if (busy) return kTRUE; /// avoid multiple execution
  busy = true;

  /// Select a table
  if (parm1 >= id_SelBut && parm1 < id_SelBut + n_table) {
    int isel = parm1 - id_SelBut;
    busy = false;
    runnoDefault = (int) runno->GetNumber();
    SelectTable(isel);
  }
  /// Entry in text field
  else if (parm1 >= id_Caldb && parm1 <= id_Caldb + id_RunNo) {
    int isel = parm1 - id_Caldb;
    switch (isel) {
    case id_RunNo:
      for (int i=0; i<n_table; i++) 
	seltab[i]->SetRunno((int) runno->GetNumber());
      Label4Table();
      break;
    default:   
      break;
    }
  }
  else { 
    switch (parm1) {
    case id_Ok:
      if (GetValues()) {
	CloseWindow();
      }
      break;
    case id_Cancel:
      CloseWindow();
      break;
    default:
      understood = false;
      break;
    }
  }

  if (!understood)
    cout << "Unknown CaldbBox Message: " 
		     << std::hex << msg << "\t" << parm1 
		     << "\t" << index << std::dec << endl;

  busy = false;

  return kTRUE;
}

CaldbBox::CaldbBox(const TGWindow *p, SConstants* gotcha_) : 
  TGTransientFrame(p, this, caldbbox_w, caldbb_height()), 
  widgets(new TList),
  busy(false), n_table(gCalib->GetNparLoad()), 
  gotcha(gotcha_) {

  int h = caldbb_height();
  TGLabel*           lab[10];
  TGHorizontalFrame* fpar[MAX_TABLE];
  TGLabel*           lssi[MAX_TABLE];
//  TGTextButton*      dpar[MAX_TABLE];

  TGHorizontalFrame* frow[8];

  TGLayoutHints* bLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | kLHintsBottom, 
		      2, 2, 2, 2 );
  widgets->Add(bLayout);
  
  TGLayoutHints* fLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY, 
		      2, 2, 2, 2 );
  widgets->Add(fLayout);

  TGLayoutHints* hLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY | kLHintsExpandX, 
		      2, 2, 2, 2 );
  widgets->Add(hLayout);

  TGLayoutHints* rLayout = 
    new TGLayoutHints(kLHintsRight | kLHintsCenterY | kLHintsExpandY, 
		      2, 2, 2, 2 );
  widgets->Add(rLayout);

  TGLayoutHints* xLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsCenterY , 6, 6, 2, 2 );
  widgets->Add(xLayout);

  TGLayoutHints* xxLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsLeft , 6, 6, 2, 2 );
  widgets->Add(xxLayout);

  TGLayoutHints* xyLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsCenterY | 
		      kLHintsExpandY, 2, 2, 2, 2 );
  widgets->Add(xyLayout);
  

  char* label [] = { "Caldb server hostname", 
		     "Caldb user", 
		     "Caldb password", 
		     "RunIndex table",
		     "Run number", 
  };

  const char* edefault [] = { "clasdb.jlab.org", "clasuser", "", 
			      this->sRunIndex.c_str(), "0"};

  TGHorizontalFrame* frmpar =
    new TGHorizontalFrame(this, caldbbox_w, h/12*7);
  widgets->Add(frmpar);
  
  TGVerticalFrame* fssi =
    new TGVerticalFrame (frmpar, caldbbox_w/2, h/12*7);
  widgets->Add(fssi);

  TGVerticalFrame* fsel =
    new TGVerticalFrame (frmpar, caldbbox_w/2, h/12*7);
  widgets->Add(fsel);

  TGHorizontalFrame* frmbase =
    new TGHorizontalFrame(this, caldbbox_w, h/12*8);
  widgets->Add(frmbase);

  TGVerticalFrame* flab =
    new TGVerticalFrame(frmbase, caldbbox_w/2, h/12*8);
  widgets->Add(flab);

  TGVerticalFrame* fent =
    new TGVerticalFrame(frmbase, caldbbox_w/2, h/12*8);
  widgets->Add(fent);

  for (int i=0; i<=id_RunNo; i++) {
    lab[i]  = new TGLabel(flab, label[i]);
    widgets->Add(lab[i]);
    frow[i] = new TGHorizontalFrame(fent, caldbbox_w/2, h/12);
    widgets->Add(frow[i]);
    if (i == id_RunNo) {
      runno = new TGNumberEntry(frow[i], runnoDefault, 
				20, id_Caldb+i, 
				TGNumberFormat::kNESInteger );
      widgets->Add(runno);
      runno->Associate(this);
      frow[i]->AddFrame(runno,xLayout);
    }
    else {
      ent[i]  = new TGTextEntry(frow[i], edefault[i], id_Caldb+i);
      widgets->Add(ent[i]);
      ent[i]->Associate(this);
      frow[i]->AddFrame(ent[i],xLayout);
    }
    flab->AddFrame(lab[i],fLayout);
    fent->AddFrame(frow[i],hLayout);
  }

  ent[id_Passwd] -> SetEchoMode(TGTextEntry::kPassword);
  

  frmbase->AddFrame(flab, fLayout);
  frmbase->AddFrame(fent, xyLayout);

  TGLabel* headssi;
  fssi->AddFrame(headssi = new TGLabel(fssi, "  "), xxLayout);
  widgets->Add(headssi);

  TGLabel* headsel;
  if(SC_Version_Flag == 2)
    fsel->AddFrame(headsel = new TGLabel(fsel, "SC_CALIBRATIONS_V2 Table"), xxLayout);
  else{fsel->AddFrame(headsel = new TGLabel(fsel, "SC_CALIBRATIONS(_V2) Table"), xxLayout);}
  widgets->Add(headsel);

  for (int i=0; i<gCalib->GetNparLoad(); i++) {
    if(SC_Version_Flag == 2){
      seltab[i] = new CaldbTabSel("SC_CALIBRATIONS_V2", 
				gCalib->GetSubsystem(i), gCalib->GetItem(i));
    }else{
      seltab[i] = new CaldbTabSel("SC_CALIBRATIONS(_V2)", 
				gCalib->GetSubsystem(i), gCalib->GetItem(i));
    }
    fpar[i] = new TGHorizontalFrame(fsel, caldbbox_w, h/12, kRaisedFrame | kDoubleBorder);
    widgets->Add(fpar[i]);
    labtab[i] = new TGLabel(fpar[i],seltab[i]->GetLabel());
    widgets->Add(labtab[i]);
    fpar[i]->AddFrame(labtab[i],xLayout);
//    dpar[i] = new TGTextButton(fpar[i], "Change", id_SelBut+i);
//    widgets->Add(dpar[i]);
//    dpar[i]->Associate(this);
//    fpar[i]->AddFrame(dpar[i],rLayout);
    lssi[i] = new TGLabel(fssi, seltab[i]->GetItem());
    widgets->Add(lssi[i]);
    fssi->AddFrame(lssi[i], fLayout);
    fsel->AddFrame(fpar[i], hLayout);
  }

  frmpar->AddFrame(fssi, fLayout);
  frmpar->AddFrame(fsel, xyLayout);
  
  AddFrame(frmpar,  xxLayout);
  AddFrame(frmbase, xyLayout);

  //------------------------------------------
  // Use default font as specified in .rootrc
  FontStruct_t labelfont;
  labelfont = gClient->GetFontByName(gEnv->GetValue("Gui.BoldFont",
  		"-adobe-helvetica-bold-r-*-*-14-*-*-*-*-*-iso8859-1"));

  // Define new graphics context. Only the fields specified in
  // fMask will be used (for default TGLabel context see 
  // http://root.cern.ch/root/html/src/TGClient.cxx.html).
  GCValues_t gval;
  gval.fMask = kGCForeground | kGCFont;
  gval.fFont = gVirtualX->GetFontHandle(labelfont);
  gClient->GetColorByName("red3", gval.fForeground);
  
  GContext_t redlabelgc = gVirtualX->CreateGC(gClient->GetRoot()->GetId(), &gval);
  //-------------------------------------------------------
 

  AddFrame(status = new TGLabel (this, "Select database and tables to get constants from Caldb", redlabelgc, labelfont), bLayout);
  widgets->Add(status);
  ButtonOkCancel* bok;
  AddFrame(bok = new ButtonOkCancel (this, caldbbox_w), bLayout);
  widgets->Add(bok);

  Label4Table();

  char wname[80];
  sprintf (wname, "%s calibration - get constants from Caldb", gCalib->GetWindowName());
  SetWindowName (wname);

  MapSubwindows();
  Layout();
  MapWindow();
  fClient->WaitFor(this);
}

CaldbBox::~CaldbBox() {
  sRunIndex = ent[id_RunIndex]->GetText();
  runnoDefault=runno->GetIntNumber();
  busy = true;
  widgets->Delete();
  delete widgets;

  for (int i=0; i<n_table; i++) 
    delete seltab[i];
}

bool CaldbBox::GetValues() {
  try {
    for (int i=0; i<n_table; i++) {
      double values[N_CHANNEL];
      if (! seltab[i]->IsSelectionDone() ) 
        seltab[i]->CommonSettings(this);
      if(seltab[i]->GetRunno() <= 55356){
        SC_Version_Flag = 1;
        seltab[i]->SetSystem("SC_CALIBRATIONS");
      }else{
        SC_Version_Flag = 2;
        seltab[i]->SetSystem("SC_CALIBRATIONS_V2");
      }
      seltab[i]->GetValues(values);
      for (int j=0; j< N_CHANNEL; j++)
	gotcha->SetParameter(j,i+gCalib->GetSaveOffs(),values[j]);
    }
  }
  catch (const char* excpt) {
    new TGMsgBox(fClient->GetRoot(), this,
		 "Caldb error", excpt, kMBIconStop);
    return false;
  }
  return true;
}

bool CaldbTabSel::GetValues(double* values) {
  if (!itemValueId) throw "CaldbBox::GetValues: itemValueId is zero";
  CaldbServer caldb(hostname, user, passwd);

//   cout << subsystem << " \t " << item << " \t " << itemValueId << endl; 

  if(runno <= 55356){
    SC_Version_Flag = 1;
    system = "SC_CALIBRATIONS";
  }else{
    SC_Version_Flag = 2;
    system = "SC_CALIBRATIONS_V2";
  }
//cout << "getting values.  runno, valueid = " << runno << ", " << itemValueId << endl;
  int itemId = caldb.GetItemId(system, subsystem, item);
  if (itemId <= 0) throw "CaldbTabSel::GetValues: invalid ItemId";

  int nread  = caldb.GetValues(itemValueId, N_CHANNEL, values);
  if(((SC_Version_Flag == 1) && (nread != 288)) || ((SC_Version_Flag != 1) && (nread != N_CHANNEL))) throw "CaldbTabSel:: not getting all values";

//   for (int i=0; i<N_CHANNEL; i++)  cout << " " << values[i];    
//   cout << endl;
//   cout << "-------------------------------" << nread << " " << itemId;
//   cout << "-------------------------------" << endl;
  
  return kTRUE;
}

void CaldbBox::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}

void CaldbBox::Label4Table() {
  for (int i=0; i<n_table; i++) 
    labtab[i]->SetText(seltab[i]->GetLabel());
}

bool CaldbBox::SelectTable(int itab) {
  try {
    seltab[itab]->SetSettings(this);
    seltab[itab]->SelectTable(this);
  }
  catch (const char* excpt) {
    new TGMsgBox(fClient->GetRoot(), this,
		 "Caldb error", excpt, kMBIconStop);
    return false;
  }
  return true;
}


//------------------ CaldbBox ----------------------------------


int selhist_height(int nrow) {
  if (nrow > MAX_ROW) nrow = MAX_ROW;
  return (nrow + 2) * 44;
}

CaldbHistory::CaldbHistory(const TGWindow *p, const TGWindow *parent_, 
			   CaldbTabSel* seltab_, TSQLResult* res):
  TGTransientFrame(p, this, 1100, selhist_height(res->GetRowCount())),
  parent(parent_), widgets(new TList), seltab(seltab_),
  nrow(res->GetRowCount()) {

  TGHorizontalFrame* frm;
  TGVerticalFrame* fv[9];
  TGHorizontalFrame* fsel[MAX_ROW];
  TGTextButton* sel[MAX_ROW];
  TGHorizontalFrame* fcan;
  TGTextButton* cancel;
  TGLabel* header[9];

  int h = selhist_height(res->GetRowCount());
  int w = 1100;
  if (nrow > MAX_ROW) nrow = MAX_ROW;

  TGLayoutHints* LXY = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | kLHintsExpandY, 
		      2, 2, 2, 2 );
  widgets->Add(LXY);

  TGLayoutHints* LY = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY, 
		      3, 1, 0, 0 );
  widgets->Add(LY);

  TGLayoutHints* LEY = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY, 
		      1, 0, 0, 0 );
  widgets->Add(LEY);

  TGLayoutHints* LCY = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY | kLHintsExpandX, 
		      1, 0, 0, 0 );
  widgets->Add(LCY);

  TGLayoutHints* LBB = 
    new TGLayoutHints(kLHintsRight | kLHintsBottom, 
		      10, 10, 2, 5 );
  widgets->Add(LBB);

  TGLayoutHints* LXB = 
    new TGLayoutHints(kLHintsLeft | kLHintsBottom | kLHintsExpandX,  
		      2, 2, 2, 2 );
  widgets->Add(LXB);

  frm = new TGHorizontalFrame(this, w, h);
  widgets->Add(frm);

  char* chead[] = {"", "index date", "table date ^", "run min", "run max", "itemValueId",
		   "index by", "table by ^", "comment" };

  int relw[9] = { 3, 9, 9, 9, 9, 9, 9, 9, 34 };

  //------------------------------------------
  // Use default font as specified in .rootrc
  FontStruct_t labelfont;
  labelfont = gClient->GetFontByName(gEnv->GetValue("Gui.BoldFont",
  		"-adobe-helvetica-bold-r-*-*-14-*-*-*-*-*-iso8859-1"));

  // Define new graphics context. Only the fields specified in
  // fMask will be used (for default TGLabel context see 
  // http://root.cern.ch/root/html/src/TGClient.cxx.html).
  GCValues_t gval;
  gval.fMask = kGCForeground | kGCFont;
  gval.fFont = gVirtualX->GetFontHandle(labelfont);
  gClient->GetColorByName("white", gval.fForeground);
  
  GContext_t whitelabelgc = gVirtualX->CreateGC(gClient->GetRoot()->GetId(), &gval);
  //-------------------------------------------------------

  for (int i=0; i<9; i++) {
    fv[i] = new TGVerticalFrame(frm, w/100*relw[i] , h, kFixedWidth);
    widgets->Add(fv[i]);
    header[i] = new TGLabel(fv[i], chead[i], whitelabelgc, labelfont, kChildFrame | kRaisedFrame | kDoubleBorder, 0x100580);
    widgets->Add(header[i]);
    fv[i]->AddFrame(header[i], LCY);
  }

  long unsigned int bgcolor[4] = {0xFFFFFF, 0xFFFFE0, 0xFFF0C0, 0xFFE0A0};
  for (int irow = 0; irow < nrow; irow++) {
    TSQLRow* row = res->Next();

    fsel[irow] = new TGHorizontalFrame(fv[0], w/100*relw[0], h/nrow, kChildFrame, bgcolor[irow%4]);
    widgets->Add(fsel[irow]);
    sel[irow] = new TGTextButton(fsel[irow], "OK", irow);
    widgets->Add(sel[irow]);
    sel[irow]->Associate(this);
    fsel[irow]->AddFrame(sel[irow],LY);

    fv[0]->AddFrame(fsel[irow], LCY);
    for (int i=0; i<8; i++) {
      lab[irow][i] = new TGLabel(fv[i+1], 
				 GetField(row,i),
				 TGLabel::GetDefaultGC()(),
				 TGLabel::GetDefaultFontStruct(),
				 kChildFrame,
				 bgcolor[irow%4]);
      widgets->Add(lab[irow][i]);
      fv[i+1]->AddFrame(lab[irow][i], LCY);
    }
  }

  for (int i=0; i<9; i++) frm->AddFrame(fv[i], (i&&i<8) ? LCY : LEY);
  AddFrame(frm, LXY);
  
  fcan = new TGHorizontalFrame(this, w, h/nrow);
  widgets->Add(fcan);
  TGLabel* info 
    = new TGLabel(fcan, "^ entry in column only if different from index column");
  fcan->AddFrame(info, LXB);
  widgets->Add(info);
  cancel = new TGTextButton(fcan, "   Cancel   ", id_Cancel);
  widgets->Add(cancel);
  cancel->Associate(this);
  fcan->AddFrame(cancel, LBB);
  AddFrame(fcan,LXB);

  string wname = seltab->system + " " + seltab->subsystem + " " + seltab->item
    + ": select values";

  if (seltab->runno) {
    ostringstream srunno;
    srunno << seltab->runno;
    wname += " for run " + srunno.str();
  }

  wname += " using RunIndex " + seltab->runindex;
  SetWindowName (wname.c_str());

  MapSubwindows();
  Layout();
  MapWindow();
  fClient->WaitFor(this);
}

CaldbHistory::~CaldbHistory() {
  widgets->Delete();
  delete widgets;
}

Bool_t CaldbHistory::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  bool understood = true;
  if (parm1 < nrow) {
    SelectionOK(parm1);
    CloseWindow();
  }
  else {
    switch (parm1) {
    case id_Cancel: 
      CloseWindow();
      break;
    default:
      understood = false;
    }
  }
  if (!understood)
    cout << "CaldbHistory: " << std::hex << msg << "\t" << parm1 
	 << "\t" << index << std::dec << endl;
  return kTRUE;
}

void CaldbHistory::SelectionOK(int index) {
  seltab->date    = string(lab[index][0]->GetText()->GetString());
  seltab->itemValueId = atoi(lab[index][4]->GetText()->GetString());
  if(seltab->itemValueId <= 55356)
    SC_Version_Flag = 1;
  else{SC_Version_Flag = 2;} 
  seltab->author  = string(lab[index][5]->GetText()->GetString());
  string xcomment = string(lab[index][7]->GetText()->GetString());
  seltab->comment = xcomment.substr(0,29);
  seltab->selectionDone = true;
}

const char* CaldbHistory::GetField(TSQLRow* row, int ifield) {
  static char s[60];
  char stemp[60];
  int idate[14] = {2, 3, 14, 4, 5, 14, 6, 7, 15, 8, 9, 16, 10, 11};
  memset (s, 0, sizeof(s));
  memset (stemp, 0, sizeof(s));
  strncpy (stemp, row->GetField(ifield), 59);
  stemp[59] = 0;
  switch (ifield) {
  case 1:
    if (! strncmp (row->GetField(0), stemp, 12)) return s;
  case 0:
    strcpy (stemp+14, "/ :");
    for (int i=0; i<14; i++) s[i] = stemp[idate[i]];
    break;
  case 6:
    if (! strcmp (row->GetField(5), stemp)) return s;
  default:
    strcpy (s, stemp);
    break;
  }
  return s;
}

void CaldbHistory::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}
