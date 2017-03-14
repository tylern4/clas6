#include "jglobal.h"
#include "JSurvey.h"
#include "SectorAxis.h"
#include "Inx.h"
#include "VLine.h"
#include "NeedInput.h"
#include "CheckinBox.h"

using namespace std;

void showsector(int sec);
void showsingle(Inx index);
void setfunctions(int iscon);

enum MenuItems_t {
   JS_FILE_OPEN,
   JS_FILE_SAVE,
   JS_FILE_SAVEHIST,
   JS_FILE_PRINT,
   JS_FILE_PRINTSETUP,
   JS_FILE_EXIT,
   JS_CALDB_GETCONST,
   JS_CALDB_CHECKIN,
   JS_HELP_CONTENTS,
   JS_HELP_SEARCH,
   JS_HELP_ABOUT   
};

char* appendStr(const char* name, const char* appendix) {
  char* s = new char[strlen(name) + strlen(appendix) + 1];
  sprintf (s, "%s%s", name, appendix);
  return s;
}

JSurvey::Histo::Histo(int isubpad_, JSurvey* super_): 
  JInteractive(), isubpad(isubpad_), super(super_) {
  char hstname[128];
  sprintf (hstname, "%s", gCalib->GetHistName(isubpad));
  histo = new TH1F(hstname, hstname, N_CHANNEL, -0.5, N_CHANNEL-0.5);
  sprintf (hstname, "%s.upd", gCalib->GetHistName(isubpad));
  hdone = new TH1F(hstname, hstname, N_CHANNEL, -0.5, N_CHANNEL-0.5);
  for (int i=0; i<4; i++) {
    sprintf (hstname, "%s.caldb%d", gCalib->GetHistName(isubpad), i);
    hcomp[i] = new TH1F(hstname, hstname, N_CHANNEL, -0.5, N_CHANNEL-0.5);
  }
  histo->SetFillColor(5);
  hdone->SetFillColor(2);
  hdone->SetFillStyle(3002);
}

void JSurvey::Histo::Dump() {
  for (int i =0; i<N_CHANNEL; i++) {
    cout.width(3);
    cout << i;
    for (int j=0; j<4; j++) {
      cout.width(18);
      cout << hcomp[j]->GetBinContent(i+1);
    }
    cout << endl;
  }
}

void JSurvey::Histo::AddHcomp(TH1F* hcomp_) {
  int i=0;
  while (i<4 && hcomp[i]) i++;
  if (i==4) return;  /// all histograms used up

  hcomp[i] = hcomp_;
}

void JSurvey::Histo::DrawPad() {
  histo-> Draw("");
  hdone-> Draw("same");
  for (int i=0; i<4; i++) {
    hcomp[i]->Draw("same");
  }
  //  if (cursor) cursor-> Draw();
  DrawBox();
}

void JSurvey::Histo::Write() {
  ShowSector(0);   //unzoom, to write the whole histogram
  histo->Write();
  hdone->Write();
}

void JSurvey::Histo::ShowSector(int sec) {
  if (sec>6 || sec<0) return;
  if (!sec) {   // UnZoom
    histo->GetXaxis()->UnZoom();
    histo->GetYaxis()->UnZoom();
  }
  else {
    int i0 = (sec-1)*N_SECTOR+1;
    int i1 = sec*N_SECTOR;
    histo->GetXaxis()->SetRange(i0,i1);
  }
}

double JSurvey::Histo::GetValue(int index) {
  double value = hdone->GetBinContent(index+1);
  if (value) return value;
  return histo->GetBinContent(index+1);
}

void JSurvey::Histo::SetValue(int index, double value, int ihist) {
  switch (ihist) {
  case 0:  histo->SetBinContent(index+1, value);  break;
  case 1:  hdone->SetBinContent(index+1, value);  break;
  case 2:  case 3:  case 4: 
  case 5:  hcomp[ihist-2]->SetBinContent(index+1, value);  break;
  }
}

void JSurvey::Histo::SetColor(int ihist, int color) {
  switch (ihist) {
  case 0:  histo->SetLineColor(color); break;
  case 1:  hdone->SetLineColor(color); break;
  case 2:  case 3:  case 4: 
  case 5:  
    hcomp[ihist-2]->SetLineColor(color);  
    hcomp[ihist-2]->SetLineWidth(2);
    hcomp[ihist-2]->SetFillStyle(0);    
    break;
  }
}

void JSurvey::Histo::PointerClick(double x, double y) {
  if (!super) return;
  int index = (int) floor (x + 0.5);
  showsingle(Inx(index));
}

JSurvey::~JSurvey() {
};

JSurvey::JSurvey(const TGWindow *p, UInt_t w, UInt_t h): 
  TGMainFrame(p, w, h+50), n(N_CHANNEL), iscon(1), currentSector(0), currentIndex(-1) {
  memset (hsurvey, 0, sizeof(hsurvey));
  memset (cursor, 0, sizeof(cursor));
  memset (jpad, 0, sizeof(jpad));

  TGCompositeFrame* jsecFrame[7];

  //-----------------------begin menu--------------------------------
  /// create menu bar
  TGLayoutHints* jMenuMainLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,
						    0, 0, 1, 1);
  TGLayoutHints* jMenuMainItemLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 4, 0, 0);
  TGLayoutHints* jMenuHelpItemLayout = new TGLayoutHints(kLHintsTop | kLHintsRight);

  TGMenuBar* jMenuMain = new TGMenuBar(this, 1, 1, kHorizontalFrame);

  TGPopupMenu* jMenuFile = new TGPopupMenu(fClient->GetRoot());
  jMenuFile->AddEntry("&Open",      JS_FILE_OPEN);
  jMenuFile->AddEntry("&Save",      JS_FILE_SAVE);
  jMenuFile->AddEntry("S&ave hist", JS_FILE_SAVEHIST);
  jMenuFile->AddEntry("&Print", JS_FILE_PRINT);
  jMenuFile->AddEntry("E&xit",  JS_FILE_EXIT);
  jMenuFile->Associate(this);
  jMenuMain->AddPopup("&File", jMenuFile, jMenuMainItemLayout);

  TGPopupMenu* jMenuCaldb = new TGPopupMenu(fClient->GetRoot());
  jMenuCaldb->AddEntry("&Get Constants", JS_CALDB_GETCONST);
  jMenuCaldb->AddEntry("&Check in",      JS_CALDB_CHECKIN);
  jMenuCaldb->Associate(this);
  jMenuMain->AddPopup("&CalDB", jMenuCaldb, jMenuMainItemLayout);

  TGPopupMenu* jMenuHelp = new TGPopupMenu(fClient->GetRoot());
  jMenuHelp->AddEntry("langhei@physics.sc.edu", 777);
  jMenuHelp->AddEntry("(803) 777 - 4811",  778);
  jMenuHelp->Associate(this);
  jMenuMain->AddPopup("&Help", jMenuHelp, jMenuHelpItemLayout);

  AddFrame(jMenuMain, jMenuMainLayout);
  //-------------------------end menu--------------------------------

  /// create Histograms
  for (int i=0; i<4; i++) {
    hsurvey[i] = new Histo(i, this);    
  }
  
  TGLayoutHints* fLayout = 
    new TGLayoutHints(kLHintsCenterX | kLHintsExpandX | kLHintsCenterY | kLHintsExpandY );

  TGTab* jtab = new TGTab(this, w, h);

  /// create the Tabs for sector 0(all)-6
  for (int i=0; i<7; i++) {
    char tabname[80] = "all stripes";
    if (i) sprintf (tabname, "sector %d", i);
    jsecFrame[i] = jtab->AddTab(tabname);

    /// create master canvas
    char padname[80];
    sprintf (padname, "pad%d", i);
    jpad[i] = new TRootEmbeddedCanvas (padname, jsecFrame[i], w, h);
    jpad[i]->GetCanvas()->cd();

    /// create sub pads
    jpad[i]->GetCanvas()->Divide(2, 2, 0.001, 0.001);
    for (int j=0; j<4; j++) {
      jpad[i]->GetCanvas()->cd(j+1);
      hsurvey[j]->DrawPad();
      /// loop over sector limits
      if (!i) for (double x0=N_SECTOR-0.5; x0<N_CHANNEL-1; x0+=N_SECTOR) {
	VLine* lsec = new VLine(x0);
	lsec->SetLineStyle(3);
	lsec->SetLineColor(1);
	lsec->SetLineWidth(1);
	lsec->Draw();
      }
      else {
	SectorAxis* ax = new SectorAxis(i, hsurvey[j]->GetXaxis());
	ax->Draw();
      }
    }

    jsecFrame[i]->AddFrame (jpad[i], fLayout);
  }

  AddFrame(jtab);

  char wname[80];
  sprintf (wname, "%s: fit results", gCalib->GetWindowName()); 
  SetWindowName(wname);
  MapSubwindows();
  Layout();
  MapWindow();
  
}

bool JSurvey::MenuSelection(Long_t menu_item) {
  switch (menu_item) {

  case JS_FILE_OPEN:
    cout << "not implemanted yet" << endl;
    break;

  case JS_CALDB_CHECKIN:
    if (gCalib->IsRelative())
      new NeedInput(fClient->GetRoot(), this, 550);
    else
      new CheckinBox(fClient->GetRoot(), 550);
    break;

  case JS_CALDB_GETCONST:
    if (iscon < 5) GetCaldbConst();
    break;

  case JS_FILE_SAVEHIST:
    WriteHistogram();
    break;

  case JS_FILE_SAVE:
    WriteValues();
    break;

  case JS_FILE_PRINT:
    printf("JS_FILE_PRINT\n");
    break;

  case JS_FILE_EXIT:
    CloseWindow(); // this also terminates theApp
    break;

  default:
    return false;
  }
  return true;
}

Bool_t JSurvey::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  bool understood = true;

  switch (GET_MSG(msg)) {

  case kC_COMMAND:
    switch (GET_SUBMSG(msg)) {

    case kCM_TAB:
      ShowSector(parm1);
      break;

    case kCM_MENUSELECT:
      /// pointer over menu entry
      break;
	       
    case kCM_MENU:
      understood = MenuSelection(parm1);
      break;

    case kCM_BUTTON:
      switch (parm1) {
      case id_needOK:
	new CheckinBox(fClient->GetRoot(), 550);
	break;
      default:
	understood = false;
	break;
      }
      break;

    default:
      understood = false;
      break;
    }
    break;

  default:
    understood = false;
    break;
  }
	
  if (!understood) 
    cout << "JSurvey::ProcessMessage - message not understood: " << std::hex << msg << " " 
	 << parm1 << " " << index << std::dec << endl;
  return kTRUE;
}

bool JSurvey::GetCaldbConst () {
  if (!gConst[iscon]) gConst[iscon] = new SConstants();
  new CaldbBox(fClient->GetRoot(), gConst[iscon]);
  if (!gConst[iscon]->IsParset()) return false;

  setfunctions(iscon);

  for (int id=0; id<N_CHANNEL; id++) {
    UpdateValues(id, iscon+1);
  }
   
  for (int i=0; i<4; i++) {
    int color[5] = {1, 4, 6, 3, 33 };
    hsurvey[i]->SetColor(iscon+1, color[iscon]);
  }

  //  hsurvey[1]->Dump();
  Update();
  iscon++;
  return true;
}

//todo: check the validity of the names (especially for timewalk)
void JSurvey::WriteValues () {
  char filename[80];
  for (int ipar=0; ipar<gCalib->GetNparSave(); ipar++) {
    sprintf (filename, "%s_%s.dat", gCalib->GetSubsystem(ipar), gCalib->GetItem());
    ofstream fo(filename, ios::out);
    for (int i=0; i<N_CHANNEL; i++) {
      fo.width(12);
      fo << gConst[0]->GetParameter(i,gCalib->GetSaveOffs()+ipar) << endl;
    }
  }
  cout << "file " << filename << " with " << n << " lines created" << endl;
}

void JSurvey::WriteHistogram () {
  char hstname [255];
  sprintf (hstname, "survey_%s", gCalib->GetFileName() );
  TFile froot("gaussfit.root", "recreate");
  // todo: write individual histograms
  for (int i=0; i<4; i++)
    hsurvey[i]->  Write();
  cout << "histogram file " << "gaussfit.root" << " created" << endl;
}

void JSurvey::ShowSector (int sec) {
  currentSector=sec;
  showsector (sec);
  for (int i=0; i<4; i++) {
    hsurvey[i]->ShowSector(sec);
  }
  Update();
}

void JSurvey::SetCursor(int index) {
  currentIndex = index;

  if (currentIndex < 0) return;
  if (currentSector && currentIndex/N_SECTOR != currentSector-1)  return;

  for (int i=0; i<4; i++) {
    SelectPad(i+1); 
    if (cursor[i]) delete cursor[i];
    cursor[i] = new TLine (index, gPad->GetUymin(), index, gPad->GetUymax());
    cursor[i]-> SetLineStyle(1);
    cursor[i]-> SetLineColor(4);
    cursor[i]-> Draw();
  }
  Update();
}

void JSurvey::Update() {
  for (int i=0; i<4; i++) {
    SelectPad(i+1);
    gPad->Modified();
  }
  jpad[currentSector]->GetCanvas()->Update();
}

void JSurvey::UpdateValues(int id, int ihist) {
  int ic[6] = {0, 0, 1, 2, 3, 4};
  double* par = gConst[ic[ihist]]->GetParameters(id);
  double* err = gConst[ic[ihist]]->GetParErrors(id);

  for (int i=0; i<3; i++) {
    int icode = gCalib->Parameter4Display(i);
    switch (icode) {
    case 0:    case 1:    case 2:    case 3:    case 4:
      hsurvey[i]->SetValue(id, par[icode], ihist);
      break;

      /// in the moment no errors from caldb
    case 10:    case 11:    case 12:    case 13:    case 14:
      if (ihist<2) hsurvey[i]->SetValue(id, err[icode-10], ihist);
      break;

    case 99:
      if(gHisto[0][id] != NULL){
        if (gHisto[0][id]->GetSum())
	     hsurvey[i]->SetValue(id, par[0]*par[2]/ gHisto[0][id]->GetSum(), ihist);
      }
      break;
    }
  }
  
  /// Update chisqare histogram
  hsurvey[3]->SetValue(id, gConst[ic[ihist]]->GetChisquare(id), ihist );
}

void JSurvey::CloseWindow() {
   // Got close message for this MainFrame. Terminate the application
   // or returns from the TApplication event loop (depending on the
   // argument specified in TApplication::Run()).

   gApplication->Terminate(0);
}

