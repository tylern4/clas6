#include "CheckinBox.h"
#include "NeedInput.h"
#include "gamecock.h"

extern Calibration*   gCalib;
extern NeedConstants* gNeed;
extern GoGamecock*    gCock;
extern int SC_Version_Flag;

using namespace std;

const char* statInfo = "Select tables for Caldb/Map, fill form carefully, and press OK";
const char* statBeg  = "Writing tables to Caldb/Map, please wait...";

int height() {
  return (gCalib->MaxTableValues()+11) * 40;
}

//------------------- CheckinSettings ---------------------------
CheckinSettings::CheckinSettings (const CheckinSettings& cs) :
  ifield(cs.ifield), ipar(cs.ipar), iserr(cs.iserr),
  min(cs.min), max(cs.max), srmin(cs.srmin), srmax(cs.srmax),
  changedFile(cs.changedFile), changedCT(cs.changedCT),
  changedCI(cs.changedCI), fileonly(cs.fileonly), nocaldb(cs.nocaldb) {
  if(min <= 55356)
    SC_Version_Flag = 1;
  else{SC_Version_Flag = 2;}
  strcpy (subsystem,    cs.subsystem);
  strcpy (item,         cs.item);
  strcpy (directory,    cs.directory);
  strcpy (filename,     cs.filename);
  strcpy (hostname,     cs.hostname);
  strcpy (user,         cs.user);
  strcpy (passwd,       cs.passwd);
  strcpy (runindex,     cs.runindex);
  strcpy (commentTable, cs.commentTable);
  strcpy (commentIndex, cs.commentIndex);
}

CheckinSettings::CheckinSettings(int ifield_): 
  ifield(ifield_), iserr(ifield%2),  
  disregard(false), changedFile(false), changedCT(false), 
  changedCI(false), fileonly(false), nocaldb(false) {
  ifield_ /= 2;
  ipar     = gCalib->GetTableIndex(ifield_);
  if (iserr)  strcpy (subsystem, gCalib->GetUncertainty(ipar));
  else        strcpy (subsystem, gCalib->GetSubsystem(ipar));
  strcpy (item, gCalib->GetItem(ifield_));
  // nonexisting dbase entry for Yoffsetu
  if ( iserr && ( gCalib->GetType()==c_atten    && ipar==0 ||
		  gCalib->GetType()==c_timewalk && ipar==3 )) {
    nocaldb  = true;
    fileonly = true;
  }
}

void CheckinSettings::DefaultFilename(char* s) {
  sprintf (s, "%s/%s_%s.dat", directory, subsystem, item);
}

void CheckinSettings::DefaultCommentIndex(char *s, const char* comment) {
  sprintf (s, "SC %s %s: %s", subsystem, item, comment);
}

void CheckinSettings::SetSettings(const CheckinBox* cb) {
  strcpy (hostname, cb->ent[idHostname]->GetText());
  strcpy (user,     cb->ent[idUser    ]->GetText());
  strcpy (passwd,   cb->ent[idPasswd  ]->GetText());
  strcpy (runindex, cb->ent[idRunIndex]->GetText());
  strcpy (directory,cb->ent[idPath    ]->GetText());
  min   = atoi (cb->ent[idMin]->GetText());
  max   = atoi (cb->ent[idMax]->GetText());
  srmin = atoi (cb->ent[idSrmin]->GetText());
  srmax = atoi (cb->ent[idSrmax]->GetText());
  if(min <= 55356)
    SC_Version_Flag = 1;
  else{SC_Version_Flag = 2;}

  if (!changedCT)   strcpy (commentTable, cb->ent[idComment]->GetText());
  if (!changedCI)   DefaultCommentIndex (commentIndex, 
					 cb->ent[idComment]->GetText());
  if (!changedFile) DefaultFilename (filename);
}

//----------------------- CheckinBox ------------------------

Bool_t CheckinBox::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  bool understood = true;

  /// set details button
  if (parm1 >= idOptBut && parm1 < idParams) {
    int isel = parm1 - idOptBut;
    checkins[isel]->SetSettings(this);
    new ItemDetails(fClient->GetRoot(), checkins[isel]);
  }
  /// checkbox  want/don't want to checkin
  else if (parm1 >= idParams && parm1 < idMysql) {
    int isel = parm1 - idParams;
    checkins[isel]->disregard = (cpar[isel]->GetState() != kButtonDown);
    if (checkins[isel]->disregard) fpar[isel]->HideFrame(dpar[isel]);
    else                           fpar[isel]->ShowFrame(dpar[isel]);
  }
  else if (parm1 >= idMysql && parm1 < idReserved) {
    int isel = parm1 - idMysql;
    switch (isel) {
    case idSrmin:
      IsNumeric(isel);
      Srmin2Srmax();
      break;
    case idSrmax:
    case idMin:
    case idMax:     IsNumeric(isel);   break;
    default:    
      /*
      cout << "Mysql" << isel << ": " 
      << std::hex << msg << "\t" << parm1 
      << "\t" << index << std::dec << endl;
      */
      break;
    }
  }
  else {
    switch (parm1) {
    case id_Ok:
      if (busy) return kTRUE; /// avoid multiple execution
      busy = true;
      if (CheckinDone()) {
	CloseWindow();
	return kTRUE;
      }
      status->SetText(statInfo);
      busy = false;
      break;
    case id_Cancel:
      CloseWindow();
      break;
    case idPathSel:
      SelectPath();
      break;
    default:
      understood = false;
      break;
    }
  }

  if (!understood)
    cout << "Unknown: " << std::hex << msg << "\t" << parm1 
	 << "\t" << index << std::dec << endl;

  return kTRUE;
}

int CheckinBox::PrepareCheckin() {
  int count = 0;
  int usedefaultcom = 0;
  for (int i=0; i<n_table; i++) {
    checkins[i]->SetSettings(this);
    if (!checkins[i]->disregard) {
      count++;
      if (!checkins[i]->changedCI || !checkins[i]->changedCT) 
	usedefaultcom++;

      if (checkins[i]->GetMinRun() > checkins[i]->GetMaxRun()) {
	new TGMsgBox(fClient->GetRoot(), this,
		     "checkin stalled", 
		     "Error validity range: #from is greater than #to",
		     kMBIconStop);
	return -2;
      }

      if (checkins[i]->GetSrmin() > checkins[i]->GetSrmax()) {
	new TGMsgBox(fClient->GetRoot(), this,
		     "checkin stalled", 
		     "Error source range: #from is greater than #to",
		     kMBIconStop);
	return -3;
      }
    }
  }
  
  if (!count) {
    new TGMsgBox(fClient->GetRoot(), this,
		 "checkin stalled", "No tables selected",
		 kMBIconStop);
    return -4;
  }

  if ( usedefaultcom && ! strlen(ent[idComment]->GetText())) {
    new TGMsgBox(fClient->GetRoot(), this,
		 "checkin stalled", "Please enter a comment",
		 kMBIconStop);
    return -5;
  }

  if ( ! strlen(ent[idSrmin]->GetText())) {
    new TGMsgBox(fClient->GetRoot(), this,
		 "checkin stalled", 
		 "Source: enter the run number of the file used for this calibration, 0 if unknown",
		 kMBIconStop);
    return -6;
  }

  return 0;
}

bool CheckinBox::CheckinDone() {
  int firstcaldb = 0;
  status->SetText(statBeg);
  if (PrepareCheckin() < 0) return false;

  /// write the Maps
  for (int i=0; i<gCalib->MaxTableValues(); i++) {
    for (int j=0; j<2; j++) {
      int k = 2*i+j;
      if(checkins[k]->GetMinRun() <= 55356)
        SC_Version_Flag = 1;
      else{SC_Version_Flag = 2;}

      if (!checkins[k]->disregard) {
	  
	if (val[k]->WriteMap(checkins[k]->GetFilename()) < 0) {
	  char msg[80];
	  sprintf(msg, "Failed to create Map file %s", checkins[k]->GetFilename());
	  new TGMsgBox(fClient->GetRoot(), this,
		       "checkin stalled", msg, kMBIconStop);
	  return false;
	}
	/// index to first table  to be checked into caldb as well?
	if (!firstcaldb && !checkins[k]->fileonly) { 
	  firstcaldb=k+1; 
	}
      }
    }
  }
  
  /// nothing into caldb? (firstcaldb contains index+1)
  if (!firstcaldb--) return true;

  /// try check into caldb database
  try {
    CaldbServer caldb(checkins[firstcaldb]->GetHostname(),
		      checkins[firstcaldb]->GetUsername(),
		      checkins[firstcaldb]->GetPassword());

    for (int i=0; i<n_table; i++) {
      if (!checkins[i]->disregard && !checkins[i]->fileonly) {
        if(checkins[i]->GetMinRun() <= 55356)
          SC_Version_Flag = 1;
        else{SC_Version_Flag = 2;}
	    
     int itemId;
     if(SC_Version_Flag == 2){
	  itemId        = caldb.GetItemId("SC_CALIBRATIONS_V2", 
					    checkins[i]->GetSubsystem(),
					    checkins[i]->GetItem());
     }else{
	  itemId        = caldb.GetItemId("SC_CALIBRATIONS", 
					    checkins[i]->GetSubsystem(),
					    checkins[i]->GetItem());
     }

	int itemValueId = caldb.WriteValues(itemId, 
					    val[i]->GetValues(), 
					    checkins[i]->GetSrmin(),
					    checkins[i]->GetSrmax(),
					    checkins[i]->GetUsername(),
					    checkins[i]->GetCommentTable());
	
	int newIndex =  caldb.WriteRunIndex(itemId, 
					    itemValueId, 
					    checkins[i]->GetMinRun(),
					    checkins[i]->GetMaxRun(),
					    checkins[i]->GetRunIndex(),
					    checkins[i]->GetUsername(),
					    checkins[i]->GetCommentIndex());
	
	char goodnews[80];
	sprintf (goodnews, "Checkin done: itemId=%d  itemValueId=%d  runIndexId=%d",
		 itemId, itemValueId, newIndex);
	gCock->AddLog(goodnews);
	cout << "successfull " << itemId << " " << itemValueId << " " << newIndex << endl; 	  
      }
    }
  }
  /// catch mysql error
  catch (const char* msg) {
    new TGMsgBox(fClient->GetRoot(), this,
		 "checkin stalled", msg, kMBIconStop);
    return false;
  }

  return true;
}

void CheckinBox::Srmin2Srmax() {
  char srcmin[80];
  strcpy (srcmin, ent[idSrmin]->GetText());
  int len = strlen(srcmin) - 1;
  if (len < 0) len=0;
  if ( (!len && ! strlen(ent[idSrmax]->GetText())) ||
       ( len && ! strncmp(srcmin, ent[idSrmax]->GetText(), len)))
    ent[idSrmax]->SetText(srcmin);
}

bool CheckinBox::IsNumeric(int ifield) {
  char s[256];
  bool numeric = true;
  strcpy (s, ent[ifield]->GetText());
  for (char* ptr=s; *ptr; ptr++) {
    if (!isdigit(*ptr)) {
      numeric = false;
      char* c0 = ptr;   /// move all char ... 
      char* c1 = ptr+1; /// ... until terminating 0
      do  *(c0++) = *(c1++);  while (*c1) ;
      *c0 = 0;
    }
  }
  if (!numeric) {
    ent[ifield]->SetText(s);
    new TGMsgBox(fClient->GetRoot(), this,
		 "input error", "must be an integer value", kMBIconStop 
		 //		 , kMBOk 
		 );  
   
    return false;
  }
  return true;
}

void CheckinBox::SelectPath() {
  static TString dir(".");
  const char *filetypes[] = { "directory", 
       "double click on selection", NULL, NULL };
  TGFileInfo fi;
  fi.fIniDir    = StrDup(dir);
  fi.fFileTypes = filetypes;
  new TGFileDialog(fClient->GetRoot(), this, kFDSave, &fi);
  if (fi.fFilename)
    ent[idPath]->SetText(fi.fIniDir);
}

CheckinBox::CheckinBox(const TGWindow *p, UInt_t w) : 
  TGTransientFrame(p, this, w, height()), 
  busy(false), n_table(gCalib->MaxTableValues()*2) {

  int h = height();
  TGLabel* lab[10];

  memset(checkins, 0, sizeof(checkins));
  memset(val,      0, sizeof(val));
  TGHorizontalFrame* frow[8];

  TGLayoutHints* bLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | kLHintsBottom, 
		      2, 2, 2, 2 );
  TGLayoutHints* fLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY, 
		      2, 2, 2, 2 );
  TGLayoutHints* hLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY | kLHintsExpandX, 
		      2, 2, 2, 2 );
  TGLayoutHints* rLayout = 
    new TGLayoutHints(kLHintsRight | kLHintsCenterY | kLHintsExpandY, 
		      2, 2, 2, 2 );
  TGLayoutHints* rrLayout = 
    new TGLayoutHints(kLHintsRight | kLHintsCenterY , 0, 6, 2, 2 );
  TGLayoutHints* xLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsCenterY , 6, 6, 2, 2 );

  TGLayoutHints* xxLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsLeft , 12, 12, 2, 2 );

  TGLayoutHints* xyLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsCenterY | 
		      kLHintsExpandY, 2, 2, 2, 2 );
  

  char* label [] = { "Caldb server hostname", 
		     "Caldb user", 
		     "Caldb password", 
		     "RunIndex table",
		     "Valid from run", 
		     "Source from run", 
		     "Comment", 
		     "Directory for Map files",
		     "to run",
		     "to run"
  };

  char* edefault [] = { "localhost", "clasuser", "", "RunIndex",
			"99999", "", "", "/tmp",
			"99999", "" };

  TGHorizontalFrame* frmbase =
    new TGHorizontalFrame(this, w, h/12*11);
  
  TGHorizontalFrame* frmpar =
    new TGHorizontalFrame(this, w, h/12*11);
  
  TGVerticalFrame* flab =
    new TGVerticalFrame(frmbase, w/2, h/12*11);

  TGVerticalFrame* fent =
    new TGVerticalFrame(frmbase, w/2, h/12*11);

  for (int i=0; i<idMax; i++) {
    lab[i]  = new TGLabel(flab, label[i]);
    frow[i] = new TGHorizontalFrame(fent, w/2, h/12);
    ent[i]  = new TGTextEntry(frow[i], edefault[i], idMysql+i);
    flab->AddFrame(lab[i],fLayout);
    frow[i]->AddFrame(ent[i],xLayout);
    fent->AddFrame(frow[i],hLayout);
  }

  lab[idMax] = new TGLabel(frow[idMin], label[idMax]);
  ent[idMax] = new TGTextEntry(frow[idMin], edefault[idMax], idMysql+idMax);
  frow[idMin] -> AddFrame(lab[idMax], fLayout);
  frow[idMin] -> AddFrame(ent[idMax], xLayout);

  lab[idSrmax] = new TGLabel(frow[idSrmin], label[idSrmax]);
  ent[idSrmax] = new TGTextEntry(frow[idSrmin], edefault[idSrmax], idMysql+idSrmax);
  frow[idSrmin] -> AddFrame(lab[idSrmax], fLayout);
  frow[idSrmin] -> AddFrame(ent[idSrmax], xLayout);

  ent[idPasswd] -> SetEchoMode(TGTextEntry::kPassword);
  
  for (int i=0; i<10; i++) {
    ent[i]->Associate(this);
  }

  TGPictureButton* bpath =
    new TGPictureButton(frow[idPath], 
			fClient->GetPicture("folder_t.xpm"), idPathSel);
  bpath->Associate(this);
  frow[idPath]->AddFrame(bpath, rrLayout);

  frmbase->AddFrame(flab, fLayout);
  frmbase->AddFrame(fent, xyLayout);

  TGVerticalFrame* fvalerr[2];

  for (int j=0; j<2; j++) {
    fvalerr[j] = new TGVerticalFrame(frmpar, w/2, h/12*11);
    fvalerr[j]->AddFrame(new TGLabel(fvalerr[j], 
				     (j? "Uncertainties:" : "Calibration Constants:")), xxLayout);
    for (int i=0; i<gCalib->MaxTableValues(); i++) {
      int k=2*i+j;

      checkins[k] = new CheckinSettings(k);

      switch (gCalib->GetType()) {
      case c_tdc:
      case c_tdcp:
	if (!k) val[k] = new CheckinValues(gCalib->IsRight(), gNeed->GetRoc());
	else    val[k] = new CheckinValues(i,j);
	break;
      case c_gmean:
	val[k] = new CheckinValues(k, gNeed->GetYoffset(0), gNeed->GetYoffset(1));
	break;
      default:
	val[k] = new CheckinValues(i,j);
	break;
      }
      
      char parname [80];
      sprintf(parname, "%s %s", checkins[k]->GetSubsystem(), checkins[k]->GetItem());
      fpar[k] = new TGHorizontalFrame(fvalerr[j], w, h/12, kRaisedFrame | kDoubleBorder);
      cpar[k] = new TGCheckButton(fpar[k], parname, idParams+k);
      cpar[k]->SetState(kButtonDown);
      cpar[k]->Associate(this);
      fpar[k]->AddFrame(cpar[k],xLayout);
      dpar[k] = new TGTextButton(fpar[k], "Details", idOptBut+k);
      dpar[k]->Associate(this);
      fpar[k]->AddFrame(dpar[k],rLayout);
      fvalerr[j]->AddFrame(fpar[k], xxLayout);
    }
    frmpar->AddFrame(fvalerr[j],xxLayout);
  }
  
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
 

  AddFrame(status = new TGLabel (this, statInfo, redlabelgc, labelfont), bLayout);
  AddFrame(new ButtonOkCancel (this, w), bLayout);
 
  char wname[80];
  sprintf (wname, "%s calibration - form for database checkin", gCalib->GetName());
  SetWindowName (wname);

  MapSubwindows();
  Layout();
  MapWindow();
}

CheckinBox::~CheckinBox() {
  delete status;
  for (int i=0; i<10; i++) {
    delete ent[i];
  }
  for (int i=0; i<n_table; i++) {
    delete dpar[i];
    delete cpar[i];
    delete fpar[i];
    delete val[i];
    delete checkins[i];
  }
}

void CheckinBox::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}

//---------------- ItemDetails --------------------------
Bool_t ItemDetails::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  bool understood = true;
  switch (parm1) {
  case id_Ok:
    AcceptSettings();
    CloseWindow();
    break;
  case id_Cancel:
    CloseWindow();
    break;
  case idOnlyF:
    if (cs->nocaldb) {
      if (onlyf->GetState() != kButtonDown) 
	onlyf->SetState(kButtonDown);
      return kTRUE;
    }
    DisableCaldbFields (onlyf->GetState() == kButtonDown);
    break;
  case idFileSel:
    SelectFile();
    break;
  default:
    understood = false;
    break;
  }

//   if (!understood) 
//     cout << "ItemDetails: " << std::hex << msg << "\t" << parm1 
// 	 << "\t" << index << std::dec << endl;
  return kTRUE;
}

ItemDetails::ItemDetails (const TGWindow* p, CheckinSettings* cs_) :
  TGTransientFrame(p, this, 550, 180), cs(cs_) {
  int w = 500;
  int h = 450;

  onlyf = new TGCheckButton(this, "Don't check into database, only create Map file", idOnlyF);
  if (cs->fileonly) onlyf->SetState(kButtonDown);

  TGHorizontalFrame* frm = new TGHorizontalFrame(this, w, h/7*6);
  TGVerticalFrame* fl = new TGVerticalFrame(frm, w/2, h/7*6);
  TGVerticalFrame* fe = new TGVerticalFrame(frm, w/2, h/7*6);
  TGHorizontalFrame* ffile;

  TGLayoutHints* bLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsExpandX | kLHintsBottom,  2, 2, 2, 2 );
  TGLayoutHints* lLayout = 
    new TGLayoutHints(kLHintsLeft  | kLHintsCenterY, 2, 2, 2, 2 );
  TGLayoutHints* rLayout = 
    new TGLayoutHints(kLHintsRight | kLHintsCenterY, 5, 0, 2, 2 );
  TGLayoutHints* tLayout = 
    new TGLayoutHints(kLHintsLeft  | kLHintsExpandX | kLHintsTop, 10, 2, 10, 2 );
  TGLayoutHints* xLayout = 
    new TGLayoutHints(kLHintsLeft  | kLHintsExpandX | kLHintsCenterY, 2, 2, 2, 2 );
  TGLayoutHints* xxLayout = 
    new TGLayoutHints(kLHintsLeft  | kLHintsExpandX | kLHintsCenterY, 0, 0, 0, 0 );
  TGLayoutHints* rxyLayout = 
    new TGLayoutHints(kLHintsRight | kLHintsExpandX | 
		      kLHintsExpandY | kLHintsCenterY, 2, 2, 2, 2 );
  TGLayoutHints* lyLayout = 
    new TGLayoutHints(kLHintsLeft | 
		      kLHintsExpandY | kLHintsCenterY, 2, 2, 2, 2 );

  //  fl->AddFrame(new TGLabel(fl, "Map file name", TGLabel::GetDefaultGC()(), 
  //                       TGLabel::GetDefaultFontStruct(), 
  //			   kChildFrame, 0xFFFF80), lLayout);
  fl->AddFrame(new TGLabel(fl, "Map file name"), lLayout);
  fl->AddFrame(labCT = new TGLabel(fl, "Comment for Constants"), lLayout);
  fl->AddFrame(labCI = new TGLabel(fl, "Comment for RunIndex"),  lLayout);


  fe->AddFrame(ffile = new TGHorizontalFrame(fe, w, h/7), xLayout);
  fe->AddFrame(entCT = new TGTextEntry(fe, cs->GetCommentTable(), idComCT), xLayout);
  fe->AddFrame(entCI = new TGTextEntry(fe, cs->GetCommentIndex(), idComCI), xLayout);

  ffile->AddFrame(efile = new TGTextEntry(ffile, cs->GetFilename(), idFile), 
		  xxLayout);
  ffile->AddFrame(bfile = new TGPictureButton(ffile, fClient->GetPicture("folder_t.xpm"), 
					      idFileSel), rLayout);
  entCT->Associate(this);
  entCI->Associate(this);
  efile->Associate(this);
  bfile->Associate(this);

  frm->AddFrame(fe, rxyLayout);
  frm->AddFrame(fl, lyLayout);

  AddFrame(onlyf, tLayout);
  AddFrame(frm, rxyLayout);
  AddFrame(new ButtonOkCancel (this, w), bLayout);

  DisableCaldbFields(cs->fileonly);

  char wname[80];
  sprintf (wname, "details for %s %s", cs->GetSubsystem(), cs->ValueError());
  SetWindowName (wname);

  MapSubwindows();
  Layout();
  MapWindow();
  fClient->WaitFor(this);
}

ItemDetails::~ItemDetails () {
  delete onlyf;
  delete efile;
  delete bfile;
  delete entCT;
  delete entCI;
  delete labCT;
  delete labCI;
}

void ItemDetails::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}

void ItemDetails::DisableCaldbFields(bool disable) {
  if (disable) {
    labCT->SetText("                     "); 
    labCI->SetText("                    "); 
    entCT->SetState(kFALSE); 
    entCI->SetState(kFALSE); 
  }
  else {
    labCT->SetText("Comment for Constants"); 
    labCI->SetText("Comment for RunIndex"); 
    entCT->SetState(kTRUE); 
    entCI->SetState(kTRUE); 
  }
}

void ItemDetails::AcceptSettings () {
  cs->fileonly = (onlyf->GetState() == kButtonDown);
  if (strcmp(cs->GetFilename(), efile->GetText())) {
    cs->SetFilename(efile->GetText());
    cs->changedFile = true;
  } 
  if (strcmp(cs->GetCommentTable(), entCT->GetText())) {
    cs->SetCommentTable(entCT->GetText());
    cs->changedCT = true;
  } 
  if (strcmp(cs->GetCommentIndex(), entCI->GetText())) {
    cs->SetCommentIndex(entCI->GetText());
    cs->changedCI = true;
  } 
}

void ItemDetails::SelectFile() {
  static TString dir(cs->GetDirectory());
  const char *filetypes[] = { "dat files", ".dat",
			      "map files", ".map",
			      NULL, NULL };
  TGFileInfo fi;
  fi.fIniDir    = StrDup(dir);
  fi.fFileTypes = filetypes;
  new TGFileDialog(fClient->GetRoot(), this, kFDSave, &fi);
  if (fi.fFilename)
    efile->SetText(fi.fFilename);
}

//---------------- ItemDetails --------------------------

