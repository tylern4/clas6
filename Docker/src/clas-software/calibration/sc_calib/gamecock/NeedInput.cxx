#include "NeedInput.h"
#include <sstream>

extern Calibration*   gCalib;
extern NeedConstants* gNeed;
extern int SC_Version_Flag;

using namespace std;

int needinp_height() {
  switch (gCalib->GetType()) {
  case c_tdcp:
  case c_tdc:   return 100;
  case c_gmean: return 150;
  default: throw "using class NeedInput for wrong calibration type";
  }
}

Bool_t NeedInput::ProcessMessage(Long_t msg, Long_t parm1, Long_t index) {
  bool understood = true;

  if (parm1 >= id_needPic && parm1 < id_needPic+numberFiles) { 
    SelectFile(parm1 - id_needPic);
  }
  else if (parm1 >= id_needEntry && parm1 < id_needEntry+numberFiles) {
  }
  else switch (parm1) {
  case id_Ok: 
    if (SelectionOK()) {
      SendMessage(parent, msg, id_needOK, index);
      CloseWindow();
    }
    break;
  case id_Cancel:
    CloseWindow();
    break;
  }

  if (!understood)
    cout << "Unknown: " << std::hex << msg << "\t" << parm1 
	 << "\t" << index << std::dec << endl;

  return kTRUE;
}

NeedInput::NeedInput(const TGWindow *p, TGWindow *parent_, UInt_t w) : 
  TGTransientFrame(p, this, w, needinp_height()), parent(parent_) {

  int h = needinp_height();
  string label[5];
  string deffile[5];
  string locString;

  switch (gCalib->GetType()) {
  case c_tdc:   
  case c_tdcp:
    if (SelectionOK()) {
      SendMessage(parent, (kC_COMMAND << 8) | kCM_BUTTON, id_needOK, 0);
    }
    return;
    numberFiles = 1;
    label[0]   = "ROC File";
    if(SC_Version_Flag == 2)
      deffile[0] = "/group/clas/parms/TT_v2";
    else{deffile[0] = "/group/clas/parms/TT";}
    break;
    
  case c_gmean: 
    numberFiles = 2;
    label[0]   = "Yoffset values";
    label[1]   = "Yoffset errors";
    locString = getenv("TOP_DIR");
    locString += "/clas/packages/utilities/sc_calib/";
    deffile[0] = locString;
    deffile[0] += "Yoffset_value.dat";
    deffile[1] = locString;
    deffile[1] += "Yoffsetu_value.dat";
    break;
  default: {
    cerr << "program error, NeedInput required for calibration step " << (int) gCalib->GetType() << endl;
    throw "using class NeedInput for wrong calibration type";
  }
  }

  TGHorizontalFrame* frow[5];

  TGLayoutHints* bLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsBottom, 
		      2, 2, 2, 2 );
  TGLayoutHints* fLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY, 
		      2, 2, 2, 2 );
  TGLayoutHints* hLayout = 
    new TGLayoutHints(kLHintsLeft | kLHintsCenterY | kLHintsExpandY | kLHintsExpandX, 
		      2, 2, 2, 2 );
//   TGLayoutHints* rLayout = 
//     new TGLayoutHints(kLHintsRight | kLHintsCenterY | kLHintsExpandY, 
// 		      2, 2, 2, 2 );
  TGLayoutHints* rrLayout = 
    new TGLayoutHints(kLHintsRight | kLHintsCenterY , 0, 6, 2, 2 );
  TGLayoutHints* xLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsCenterY , 6, 6, 2, 2 );

//   TGLayoutHints* xxLayout = 
//     new TGLayoutHints(kLHintsExpandX | kLHintsLeft , 12, 12, 2, 2 );

  TGLayoutHints* xyLayout = 
    new TGLayoutHints(kLHintsExpandX | kLHintsCenterY | 
		      kLHintsExpandY, 2, 2, 2, 2 );
  

  TGHorizontalFrame* frm =
    new TGHorizontalFrame(this, w, h/12*11);
  
  TGVerticalFrame* flab =
    new TGVerticalFrame(frm, w/2, h/12*11);

  TGVerticalFrame* fent =
    new TGVerticalFrame(frm, w/2, h/12*11);

  for (int i=0; i<numberFiles; i++) {
    lab[i]   = new TGLabel(flab, label[i].c_str());
    frow[i]  = new TGHorizontalFrame(fent, w/2, h/12);
    ent[i]   = new TGTextEntry(frow[i], deffile[i].c_str(), id_needEntry+i);
    pic[i]   = new TGPictureButton(frow[i], 
	   fClient->GetPicture("folder_t.xpm"), id_needPic+i);

    flab->AddFrame(lab[i],fLayout);
    frow[i]->AddFrame(ent[i], xLayout);
    frow[i]->AddFrame(pic[i], rrLayout);
    fent->AddFrame(frow[i],hLayout);
  }
  
  for (int i=0; i<numberFiles; i++) {
    ent[i]->Associate(this);
    pic[i]->Associate(this);
  }


  frm->AddFrame(flab, fLayout);
  frm->AddFrame(fent, xyLayout);

  AddFrame(frm, xyLayout);

  ButtonOkCancel* fbut = new ButtonOkCancel (this, w);
  AddFrame(fbut, bLayout);
 
  SetWindowName("Need additional information to create data sets");
  MapSubwindows();
  Layout();
  MapWindow();
}

NeedInput::~NeedInput() {
  for (int i=0; i<numberFiles; i++) {
    delete lab[i];
    delete ent[i];
    delete pic[i];
  }
}

void NeedInput::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}

bool NeedInput::SelectionOK() {
  try {
    switch (gCalib->GetType()) {
    case c_tdc: 
    case c_tdcp:     
      {
      BankDescription *bd;
      if(SC_Version_Flag == 2){
        bd = new BankDescription(string("SCT 684 1 6 1 57 1 1190  2  layer2lr=1" ));
      }else{
        bd = new BankDescription(gCalib->GetType() == c_tdc ? 
			  string("SC  576 1 6 1 48 0 1872  5  leftright=1") :
			  string("SCT 576 1 6 1 48 1 1190  2  layer2lr=1" ) );
      }
	gNeed = new NeedConstants(bd); 
      }
      break;
    case c_gmean: 
      gNeed = new NeedConstants(ent[0]->GetText(),ent[1]->GetText());
      break;
    default: throw "using class NeedInput for wrong calibration type";
    }
  }
  catch (const char* excpt) {
    new TGMsgBox(fClient->GetRoot(), this, "input file error", excpt, kMBIconStop);
    return false;
  }
  return true;
}

void NeedInput::SelectFile(int ient) {
  static TString dir;
  const char* filetypes_tdc[] = { 
    "roc files", "ROC*",
    "all files", "*",
    NULL, NULL };
  const char* filetypes_atten[] = {
    "dat files", "*.dat", 
    "map files", "*.map",
    "all files", "*",
    NULL, NULL };

  TGFileInfo fi;

  switch (gCalib->GetType()) {
  case c_tdc:
    if(SC_Version_Flag == 2)
      dir = TString("/group/clas/parms/TT_v2");
    else{dir = TString("/group/clas/parms/TT");}
    fi.fFileTypes = filetypes_tdc;
    break;
  case c_gmean:
    dir = TString("/tmp");
    fi.fFileTypes = filetypes_atten;
    break;
  default: /// make the compiler happy
    break;
  }
  fi.fIniDir    = StrDup(dir);
  new TGFileDialog(fClient->GetRoot(), this, kFDOpen, &fi);
  if (fi.fFilename)
    ent[ient]->SetText(fi.fFilename);
}


///----------------------- NeedConstants --------------------------------
NeedConstants::NeedConstants(BankDescription* bd) {
  Yoffset[0] = NULL;
  Yoffset[1] = NULL;
  rroc = new ReadRoc(bd);
}

NeedConstants::NeedConstants(const char* yoffname, const char* yerrname) :
  rroc(NULL) {
  Yoffset[0] = new double[N_CHANNEL];
  Yoffset[1] = new double[N_CHANNEL];

  ifstream fval(yoffname);
  ifstream ferr(yerrname);
  int locBreakFlag = 0;
  for (int i=0; i<N_CHANNEL; i++) {
    if(!fval.good()){
      cout << "Error reading Yoffset value file. yoffname, i = " << yoffname << ", " << i << endl;
      if(i >= 288){
        cout << "Assuming runno < 55357, sorting constants" << endl;
        locBreakFlag = 1;
      }
      break;
    }
    fval >> Yoffset[0][i];
    if(!ferr.good()){
      cout << "Error reading Yoffset error file. yerrname, i = " << yerrname << ", " << i << endl;
      if(i >= 288){
        cout << "Assuming runno < 55357, sorting constants" << endl;
        locBreakFlag = 1;
      }
      break;
    }
    ferr >> Yoffset[1][i];
  }
  if(locBreakFlag == 0)
    return;
  int loc_i, loc_j, locSourceIndex, locPlaceIndex;
  //if here, then only 288 constants, rearrange them to insert gaps for 49->57 for each sector
  for(loc_i = 5; loc_i >= 1; loc_i--){
    for(loc_j = 47; loc_j >= 0; loc_j--){
      locPlaceIndex = 57*loc_i + loc_j;
      locSourceIndex = 48*loc_i + loc_j;
      Yoffset[0][locPlaceIndex] = Yoffset[0][locSourceIndex];
      Yoffset[1][locPlaceIndex] = Yoffset[1][locSourceIndex];
    }
  }
}

NeedConstants::~NeedConstants() {
  if (rroc)       delete rroc;
  if (Yoffset[0]) delete Yoffset[0];
  if (Yoffset[1]) delete Yoffset[1];
}

///----------------------- NeedConstants --------------------------------
