#include <stdio.h>
#include "gamecock.h"
#define DEFINE_GLOBAL
#include "jglobal.h"

int SC_Version_Flag = 1;

enum ETestCommandIdentifiers {
   M_FILE_NEW,
   M_FILE_OPEN,
   M_FILE_SAVE,
   M_FILE_SAVEAS,
   M_FILE_CLOSE,
   M_FILE_PRINT,
   M_FILE_PRINTSETUP,
   M_FILE_EXIT,

   M_SETTINGS_SETTINGS,   
   
   M_HELP_CONTENTS,
   M_HELP_ABOUT,
   
   id_Go_create,
   id_Go_fit,
   id_OpenData_create,
   id_OpenData_fit,
   id_gamecock,
   id_program_location,
   id_output_location   
};

TString extOutputPipe  ("/tmp/gamecock.out_$(USER).pipe");
TString extInputPipe   ("/tmp/gamecock.inp_$(USER).pipe");
TString extShellScript ("/tmp/gamecock.$(USER).rc");

//const char *Option_get[MAX_RADIO] = { "atten", "gmean", "mass", "p2pdelay", "veff", "tdc left", "tdc right","timewalk left","timewalk right"};
const char *Option_get[MAX_RADIO_create] = { "atten", "gmean", "p2pdelay_el", "veff", "tdc", "timewalk_l", "timewalk_r"};

const char *Execute_external_command[MAX_RADIO_create] = { 
  "$TOP_DIR/bin/$OS_NAME/tof_calib", 
  "$TOP_DIR/bin/$OS_NAME/gmean_cooked", 
  "$TOP_DIR/bin/$OS_NAME/p2p_delay_el", 
  "$TOP_DIR/bin/$OS_NAME/veff_calib", 
  "$TOP_DIR/bin/$OS_NAME/tdc_calib", 
  "$TOP_DIR/bin/$OS_NAME/make_tw_histos",
  "$TOP_DIR/bin/$OS_NAME/make_tw_histos"
};

const char* Execute_external_option[MAX_RADIO_create] = {
  "-G",                         // atten 
  "-G",                         // gmean
  "-G -s",                      // p2p_delay_el
  "-G",                         // veff
  "-G -X",                      // tdc
  "-G -f -l",                          // timewalk left
  "-G -f -r"                          // timewalk right
};  

const char *Option_fit[MAX_RADIO_fit] = { "atten", 
					  "gmean", 
					  "mass", 
					  "pedestal", 
					  "p2p electr", 
					  "p2p photon", 
					  "veff", 
					  "tdc fastb left", 
					  "tdc fastb right",
					  "tdc pipln left", 
					  "tdc pipln right",
					  "timewalk left",
					  "timewalk right"};

// const calibration_t selCalib_create[MAX_RADIO_create] = { c_atten, c_gmean, c_mass_p2pdelay, c_veff,
// 					    c_tdc, c_timewalk };

const calibration_t selCalib[MAX_RADIO_fit] = { c_atten, 
						c_gmean, 
						c_mass, 
						c_mass,
						c_p2pdelay, 
						c_p2pdelay, 
						c_veff,
						c_tdc, 
						c_tdc, 
						c_tdcp, 
						c_tdcp, 
						c_timewalk, 
						c_timewalk };
					    
// const item_t selItem_create[MAX_RADIO_create] = {  i_unknown,  i_unknown,  i_unknown,  i_unknown,                                                i_unknown, i_unknown };

const item_t selItem[MAX_RADIO_fit] = { i_unknown,  
					i_unknown,  
					i_unknown,  
					i_unknown,  
					i_unknown,  
					i_unknown,  
					i_unknown, 
					i_left, 
					i_right,  
					i_left, 
					i_right,  
					i_left, 
					i_right };

const char *filetypes_create[] = { "All files",     "*",
			    "Data files",    "*.A[0-9][0-9]",
			    "Data files",    "*.a[0-9][0-9]",
			    "Data sets",     "*.set",
			    0,               0 };
			    
const char *filetypes_fit[] = { "All files",     "*",
			   "ROOT files",    "*.root",
                            "HBOOK files",   "*.hbook",
                            0,               0 };


TRint*        theApp;

/// add path name to resource file name
class GamecockResource {
  string s;
public:
  GamecockResource(string filename);
  operator const char*() const { return s.c_str(); }
};


/// add path name to resource file name
GamecockResource::GamecockResource(string filename) {
  //   1. environment string user setting 
  if (getenv("GAMECOCK_RESOURCE")) {
    s = string(getenv("GAMECOCK_RESOURCE")) + "/" + filename;
  }
  //   2. provided from Makefile -DGAMECOCK_RESOURCE=...
  else {
#ifdef GAMECOCK_RESOURCE
    s = string(GAMECOCK_RESOURCE) + "/" + filename;
#else
    // 3. try to extract from current file name
    string path(__FILE__);
    int islash = path.rfind("/");
    if (islash == string::npos) {  // no slash found
      s = filename;
    }                              // cut at slash
    else {
      s =  path.substr(0,islash)  + "/" + filename;
    }
#endif
  }
}

//------------ prototype for user routine ----------
#include <signal.h>
void StopIt (int signal){
  cout<< "Got a signal "<< signal << ": Stop it"<<endl;
  exit (0);
}

void showall();

//------------ end prototype -----------------------
Startanim::Startanim(GoGamecock* super_): TObject(), count(8), super(super_) {
  timer = new TTimer(200);         // every 500 ms
  timer->SetObject(this);         // call HandleTimer()
}

void Startanim::TurnOn() {
  timer->TurnOn();                // from now on
}

Bool_t Startanim::HandleTimer(TTimer* timer_) {
  if (count <= 0) return false;
  super->SwitchPicture(count--);
  if (count <= 0) timer->TurnOff();
  return false;
}


GoGamecock::GoGamecock(const TGWindow *p, UInt_t w, UInt_t h)
  : TGMainFrame(p, w, h, kVerticalFrame), WhichButton1(-1), WhichButton2(-1),
    islog(false)
{

// set up some font and color for late use
	//Use default font as specified in .rootrc
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
//=================================================================

  //Create main frame. A TGMainFrame is a top level window.

  //   fMain   = new TGMainFrame(p, w, h, kVerticalFrame);
  //   fMain->Connect("CloseWindow()", "GoGamecock", this, "CloseWindow()");
   
   // Create menubar and popup menus. The hint objects are used to place
   // and group the different menu widgets with respect to eachother.
   fMenuBarLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,
                                      0, 0, 1, 1);
   fMenuBarItemLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 4, 0, 0);
   fMenuBarHelpLayout = new TGLayoutHints(kLHintsTop | kLHintsRight);

   fMenuFile = new TGPopupMenu(gClient->GetRoot());
   fMenuFile->AddEntry("&New...", M_FILE_NEW);
   fMenuFile->AddEntry("&Open...", M_FILE_OPEN);
   fMenuFile->AddEntry("&Close", M_FILE_CLOSE);
   fMenuFile->AddSeparator();
   fMenuFile->AddEntry("&Print", M_FILE_PRINT);
   fMenuFile->AddEntry("P&rint setup...", M_FILE_PRINTSETUP);
   fMenuFile->AddSeparator();
   fMenuFile->AddEntry("E&xit", M_FILE_EXIT);

   //fMenuFile->HideEntry(M_FILE_PRINT);
   
   fMenuSettings = new TGPopupMenu(gClient->GetRoot());
   fMenuSettings->AddLabel("Set Create and Fit Options");
   fMenuSettings->AddSeparator();
   fMenuSettings->AddEntry("&Settings", M_SETTINGS_SETTINGS);
   
   fMenuHelp = new TGPopupMenu(gClient->GetRoot());
   fMenuHelp->AddEntry("&Contents", M_HELP_CONTENTS);
   //fMenuHelp->AddSeparator();
   fMenuHelp->AddEntry("&About", M_HELP_ABOUT);
   
   fMenuFile->Associate(this);
   fMenuSettings->Associate(this);
   fMenuHelp->Associate(this);

   fMenuBar = new TGMenuBar(this, 1, 1, kHorizontalFrame);
   fMenuBar->AddPopup("&File", fMenuFile, fMenuBarItemLayout);
   fMenuBar->AddPopup("&Settings", fMenuSettings, fMenuBarItemLayout);
   fMenuBar->AddPopup("&Help", fMenuHelp, fMenuBarHelpLayout);

   AddFrame(fMenuBar, fMenuBarLayout);

//creat a Horizontal frame to contain the Editor and a Vertical frame
//...................................................................................................
   fL_expandXY = new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 3, 3, 3, 3);
   fL_expandX = new TGLayoutHints(kLHintsExpandX, 3, 3, 3, 3);
   fL_left_expandX = new TGLayoutHints(kLHintsLeft | kLHintsExpandX);
   //fL_expandX = new TGLayoutHints(kLHintsCenterX, 3, 3, 3, 3);
   //fL_left_expandY_centerY = new TGLayoutHints(kLHintsCenterX | kLHintsCenterY);
   fL_left_expandY_centerY = new TGLayoutHints(kLHintsLeft | kLHintsExpandY |kLHintsCenterY,5,5,5,5);
   
   hframe = new TGHorizontalFrame(this,w,h);
   // AddFrame(hframe, new TGLayoutHints(kLHintsCenterX,2,2,2,2));
   AddFrame(hframe, fL_expandXY);
//...................................................................................................

// Create an editor for the Cocky picture and to show the log file
//...................................................................................................
   vframe_left = new TGVerticalFrame(hframe, w/2, h);
   hframe->AddFrame(vframe_left, fL_expandXY);
   
   //Cocky picture
   for (int i=0; i<9; i++) {
     char picname[80];
     sprintf (picname, "gc%d.xpm", i+1);
     fPic[i] = gClient->GetPicture(GamecockResource(picname));
     fImageMap[i] = new TGImageMap(vframe_left,fPic[i]);
     vframe_left->AddFrame(fImageMap[i], fL_expandXY);
   }

   	//The log file in an editor
   	fEdit = new TGTextView(vframe_left, w/2, h, kSunkenFrame | kDoubleBorder);
   	vframe_left->AddFrame(fEdit, fL_expandXY);
	
    // The Progress Bar
      fHProgress = new TGHProgressBar(vframe_left, TGProgressBar::kStandard, w/2);
      fHProgress->SetBarColor("yellow");
      fHProgress->ShowPosition();
      fHProgress->SetFillType(TGProgressBar::kBlockFill);
      vframe_left->AddFrame(fHProgress, new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,5,5,5,10));
//....................................................................................................................



// Create a Vertical frame at the right side to contain the create and fit functions which are in group frames
//.............................................................................................................................................	
	vframe_right = new TGVerticalFrame(hframe,w/2,h);
	hframe->AddFrame(vframe_right, fL_expandXY);
        
		
   //the vertical Create frame 
   //.............................................................................................................................................
	vframe_create = new TGGroupFrame(vframe_right, "Create histograms", kVerticalFrame);
	//vframe_create = new TGGroupFrame(vframe, "Create histograms", kHorizontalFrame);
	//vframe_right->AddFrame(vframe_create, new TGLayoutHints(kLHintsNormal,5,5,1,1));
	vframe_right->AddFrame(vframe_create, fL_expandX);
	
	// Create a horizontal frame in vframe_create to include "open file" box and button 
	//.........................................................................................................................
	hframe_create_file = new TGHorizontalFrame(vframe_create,210,30);
	vframe_create->AddFrame(hframe_create_file, fL_expandX);

	// set a label and a box for the data file 
	//TGTextButton *data = new TGTextButton(vframe,"&Data",id_OpenData);
	
	vframe_create_file_label = new TGVerticalFrame(hframe_create_file,200,40);
	hframe_create_file->AddFrame(vframe_create_file_label, fL_left_expandX);
   
	lab_create = new TGLabel(vframe_create_file_label, "Data File:",redlabelgc, labelfont);
	//lab->Resize(lab->GetDefaultWidth(), 30);
	vframe_create_file_label->AddFrame(lab_create,  new TGLayoutHints(kLHintsLeft,5,5,5,5));    
	//lab->SetHeight(10);
	
	//add a text box to show the file name
	fdatafile_create = new TGTextEntry(vframe_create_file_label, fTbdatafile_create = new TGTextBuffer(100));
	fdatafile_create->Associate(this);
	//fdatafile->Resize(200, fdatafile->GetDefaultHeight());
	//fdatafile_create->Resize(200,25);
	fdatafile_create->DrawBorder();
	//hframe_create_file->AddFrame(fdatafile_create, new TGLayoutHints(kLHintsLeft,20,5,5,5));
	vframe_create_file_label->AddFrame(fdatafile_create, new TGLayoutHints(kLHintsExpandX,5,5,5,5));
	
	//add a button to open data file
	data_create = new TGPictureButton(hframe_create_file, 
					  fClient->GetPicture(GamecockResource("open.xpm")),
					  id_OpenData_create);
	data_create->Associate(this);
	hframe_create_file->AddFrame(data_create, new TGLayoutHints(kLHintsRight | kLHintsCenterY,30,5,5,5));
	
	
	//.........................................................................................................................
	
	
	// create some radio buttons to choose which calibaration you want to do
	//  and set up a "Create" button to run calibaration

	//bg_create = new TGGroupFrame(vframe_create, "", kVerticalFrame);
	bg_create = new TGGroupFrame(vframe_create, "", kHorizontalFrame);
	//vframe_create->AddFrame(bg_create, new TGLayoutHints(kLHintsTop,1,1,1,1));
	vframe_create->AddFrame(bg_create, fL_expandX);

	for (int j = 0; j < 3; ++j) {	
		smallbox_create[j] = new TGVerticalFrame(bg_create,10,10);
		bg_create->AddFrame(smallbox_create[j], new TGLayoutHints(kLHintsLeft,5,5,20,20));
		
          for (int i = 0; i < 3; i++){
//		for (int i = 0; i < MAX_RADIO_create/3; ++i){
//			int k=j*MAX_RADIO_create/3 + i;
               int k = 3*j + i;
               if(k >= MAX_RADIO_create)
                 break;
			fRget[k] = new TGRadioButton(smallbox_create[j], new TGHotString(Option_get[k]), k+10);
			smallbox_create[j]->AddFrame(fRget[k], fL_left_expandY_centerY);
			fRget[k]->Associate(this);   
		}
	}

	go_create = new TGPictureButton(bg_create, fClient->GetPicture(GamecockResource("gohist.xpm")) ,id_Go_create);
	go_create->Associate(this);
	bg_create->AddFrame(go_create, new TGLayoutHints(kLHintsRight | kLHintsCenterY ,5,5,30,30));
	go_create->SetToolTipText("Create histograms from data, calibration and input data file must be selected", 500);
   //.............................................................................................................................................
   
   
   //the vertical Fit frame 
   //.............................................................................................................................................
   
	vframe_fit = new TGGroupFrame(vframe_right, "Fit histograms", kVerticalFrame);
	//vframe_right->AddFrame(vframe_fit, new TGLayoutHints(kLHintsTop,5,5,1,1));
	vframe_right->AddFrame(vframe_fit, fL_expandX);
	
	
	// Create a horizontal frame in vframe_create to include "open file" box and button 
	//.........................................................................................................................
	hframe_fit_file = new TGHorizontalFrame(vframe_fit,210,30);
	vframe_fit->AddFrame(hframe_fit_file, fL_expandX);

	// set a label and a box for the data file 
	//TGTextButton *data = new TGTextButton(vframe,"&Data",id_OpenData);
	
	vframe_fit_file_label = new TGVerticalFrame(hframe_fit_file,200,40);
	hframe_fit_file->AddFrame(vframe_fit_file_label, fL_left_expandX);
   
	lab_fit = new TGLabel(vframe_fit_file_label, "Histogram File:",redlabelgc, labelfont);
	//lab->Resize(lab->GetDefaultWidth(), 30);
	vframe_fit_file_label->AddFrame(lab_fit,  new TGLayoutHints(kLHintsLeft,5,5,5,5));    
	//lab->SetHeight(10);
	
	//add a text to show file name
	fdatafile_fit = new TGTextEntry(vframe_fit_file_label, fTbdatafile_fit = new TGTextBuffer(100));
	fdatafile_fit->Associate(this);
	//fdatafile->Resize(200, fdatafile->GetDefaultHeight());
	fdatafile_fit->Resize(200,25);
	fdatafile_fit->DrawBorder();
	vframe_fit_file_label->AddFrame(fdatafile_fit, new TGLayoutHints(kLHintsExpandX,5,5,5,5));

        //add a button to open data file
	data_fit = new TGPictureButton(hframe_fit_file, fClient->GetPicture(GamecockResource("open.xpm")),id_OpenData_fit);
	data_fit->Associate(this);
	hframe_fit_file->AddFrame(data_fit, new TGLayoutHints(kLHintsCenterY | kLHintsRight,30,5,5,5));
	
	
	//.........................................................................................................................
	
	
	// create some radio buttons to choose which fiiting you want to do
	//  and set up a "Fit" button to run calibaration

	//hframe2 = new TGHorizontalFrame(vframe,300,30);
	//vframe->AddFrame(hframe2, fL_expandX);
	
	bg_fit = new TGGroupFrame(vframe_fit, "", kHorizontalFrame);
	//bg_fit = new TGGroupFrame(vframe_fit, "Fit histograms", kVerticalFrame);
	//vframe_fit->AddFrame(bg_fit, new TGLayoutHints(kLHintsTop,5,5,1,1));
	vframe_fit->AddFrame(bg_fit, fL_expandX);

	int rows = MAX_RADIO_fit/3;
	if (MAX_RADIO_fit%3) rows++;

	for (int j = 0; j < 3; ++j) { 
	  smallbox_fit[j] = new TGVerticalFrame(bg_fit,20,10);
	  bg_fit->AddFrame(smallbox_fit[j], new TGLayoutHints(kLHintsCenterX,5,5,20,20));
	  
	  for (int i = 0; i < rows; ++i) {
	    int k=j*rows + i;
	    if (k < MAX_RADIO_fit) {
	      fRfit[k] = new TGRadioButton(smallbox_fit[j], new TGHotString(Option_fit[k]), k);
	      smallbox_fit[j]->AddFrame(fRfit[k], fL_left_expandY_centerY);
	      fRfit[k]->Associate(this);
	    }
	  }
	}
	
		
	go_fit = new TGPictureButton(bg_fit, fClient->GetPicture(GamecockResource("gofit.xpm")), id_Go_fit);
	go_fit->Associate(this);
	bg_fit->AddFrame(go_fit, new TGLayoutHints(kLHintsRight | kLHintsCenterY,5,5,30,30));
	go_fit->SetToolTipText("Do the fit and database checkin, calibration must be selected, histogram is optional", 500);
   
	//      fR[0]->SetState(kButtonDown);
	//      whichkind = calib[0];
    //....................................................................................................................................
//................................................................................................................................................



// add "exit" button
//....................................................................................................................................
/*
    //exit = new TGPictureButton(hframe, fClient->GetPicture(GamecockResource("exit.xpm")),"gApplication->Terminate(0)");
    exit = new TGPictureButton(hframe, fClient->GetPicture(GamecockResource("exit.xpm")), id_GoProgress);
    exit->Associate(this);
    hframe->AddFrame(exit, new TGLayoutHints(kLHintsRight | kLHintsBottom,2,2,3,4));*/
//....................................................................................................................................


// Set a name to the main frame and do other things
//.............................................................................................................................
   SetWindowName("Gamecock");

   MapSubwindows();

   // we need to use GetDefault...() to initialize the layout algorithm...
   //   Resize(GetDefaultSize());
   //Resize(400, 200);
   Layout();

   MapWindow();

   for (int i=0; i<8; i++) vframe_left-> HideFrame(fImageMap[i]);
   vframe_left->HideFrame(fEdit);
   Startanim* sanim = new Startanim(this);
   sanim->TurnOn();
}

GoGamecock::~GoGamecock()
{
  cout << "Clean up temporary files" << endl;
  // clean up temporary files
  if (! extOutputPipe.Contains("$(USER)") )
    gSystem->Exec (TString("rm -rf ") + extOutputPipe);
  if (! extInputPipe.Contains("$(USER)") )
    gSystem->Exec (TString("rm -rf ") + extInputPipe);
  if (! extShellScript.Contains("$(USER)") )
    gSystem->Exec (TString("rm -rf ") + extShellScript);


   // Delete all created widgets.

#ifndef USE_OLD_ROOTVERSION  
    Cleanup();
#endif  
}

void GoGamecock::AddLog(const char* logline) {
  fEdit->AddLine(logline);
}

void GoGamecock::SwitchPicture(int ipic) {
  if (ipic < 0 || islog) return; /// nothing to do
  vframe_left->HideFrame(fImageMap[ipic--]);
  if (ipic < 0) {
    vframe_left->ShowFrame(fEdit);
    islog = true;
  }
  else
    vframe_left->ShowFrame(fImageMap[ipic]);
}

void GoGamecock::CloseWindow()
{
   // Got close message for this MainFrame. Terminate the application
   // or returns from the TApplication event loop (depending on the
   // argument specified in TApplication::Run()).

   gApplication->Terminate(0);
}

Bool_t GoGamecock::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
   // Handle messages send to the GoGamecock object. E.g. all menu button
   // messages.

   switch (GET_MSG(msg)) {

      case kC_COMMAND:
         switch (GET_SUBMSG(msg)) {

            case kCM_BUTTON:
	       //printf("Button was pressed, id = %ld\n", parm1);
	       switch (parm1) {
	          case id_Go_create:
	            get();
		    break;
	          case id_Go_fit:
	            fit();
		    break;
	          case id_OpenData_create:
		    OpenData_create();
		    break;
		  case id_OpenData_fit:
		    OpenData_fit();
		  default:
		    break;
	       }	    
               break;
	    case kCM_RADIOBUTTON:
               if (parm1 >= 10 && parm1 < 10 + MAX_RADIO_create) {
                  for (int i=0; i<MAX_RADIO_create; ++i)
                     if (fRget[i]->WidgetId() != parm1)
		        fRget[i]->SetState(kButtonUp);
		  WhichButton1=parm1-10;
               }
	       
	       if (parm1 >= 0 && parm1 < MAX_RADIO_fit) {
                  for (int i=0; i<MAX_RADIO_fit; ++i)
                     if (fRfit[i]->WidgetId() != parm1)
		        fRfit[i]->SetState(kButtonUp);
		  WhichButton2=parm1;
               }
               break;

            case kCM_MENUSELECT:
               //printf("Pointer over menu entry, id=%ld\n", parm1);
               break;

            case kCM_MENU:
	       HandleMenu(parm1);
               break;
            default:
               break;
         }
      default:
         break;
   }
   return kTRUE;
}

void GoGamecock::HandleMenu(Int_t id)
{
   // Handle menu items.

   switch (id) {

      case M_FILE_NEW:
	fEdit->Clear();
	 SetTitle();
         break;
	 
      case M_FILE_OPEN:
         {
            static TString dir(".");
            TGFileInfo fi;
            fi.fFileTypes = filetypes_fit;
            fi.fIniDir    = StrDup(dir.Data());
            printf("fIniDir = %s\n", fi.fIniDir);
            new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fi);
            printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
            dir = fi.fIniDir;
	    fEdit->LoadFile(fi.fFilename);
	    SetTitle();
         }
         break;

      case M_FILE_CLOSE:
	{
	  //	  printf("Close file: %s \n", file);
	  //	  fEdit->SavedAs();
	   fEdit->Clear();
	  SetTitle();
	  break;
	}
      case M_FILE_PRINT:
         printf("M_FILE_PRINT\n");
         printf("Hiding itself, select \"Print Setup...\" to enable again\n");
         fMenuFile->HideEntry(M_FILE_PRINT);
         break;

      case M_FILE_PRINTSETUP:
         printf("M_FILE_PRINTSETUP\n");
         printf("Enabling \"Print\"\n");
         fMenuFile->EnableEntry(M_FILE_PRINT);
         break;

      case M_FILE_EXIT:
	 gApplication->Terminate(0);      
	 break;
	 
      case M_SETTINGS_SETTINGS:
         new SettingsWindow(fClient->GetRoot(), this, 800, 600);
         //new SettingsWindow(this, 800, 600);
         break;
	 
      case M_HELP_CONTENTS:
	 break;

      case M_HELP_ABOUT:
	 new AboutWindow(this, 600, 400);
	 break;

      default:
         printf("Menu item %d selected\n", id);
         break;
   }
}


void GoGamecock::SetTitle()
{
   // Set title in editor window.

//   TGText *txt = GetEditor()->GetText();
//   Bool_t untitled = !strlen(txt->GetFileName()) ? kTRUE : kFALSE;
  
    char title[256];
//   if (untitled)
    sprintf(title, "Gamecock - Untitled");
//   else
//     sprintf(title, "Gamecock - %s", txt->GetFileName());
  
   SetWindowName(title);
   SetIconName(title);
}

void GoGamecock::OpenData_create()
{
            static TString dir(".");
            TGFileInfo fi;
            fi.fFileTypes = filetypes_create;
            fi.fIniDir    = StrDup(dir.Data());
            printf("fIniDir = %s\n", fi.fIniDir);
            new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fi);
            //printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
            dir = fi.fIniDir;
	    //datafile = fi.fFilename;
	    printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
	    fTbdatafile_create->Clear();
	    if (fi.fFilename)
	      fdatafile_create->SetText(fi.fFilename);
	    //fTbdatafile->AddText(0, fi.fFilename);
	    //fdatafile->Layout();
	    
}


void GoGamecock::OpenData_fit()
{
            static TString dir(".");
            TGFileInfo fi;
            fi.fFileTypes = filetypes_fit;
            fi.fIniDir    = StrDup(dir.Data());
            printf("fIniDir = %s\n", fi.fIniDir);
            new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fi);
            //printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
            dir = fi.fIniDir;
	    //datafile = fi.fFilename;
	    printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
	    fTbdatafile_fit->Clear();
	    if (fi.fFilename)
	      fdatafile_fit->SetText(fi.fFilename);
	    //fTbdatafile->AddText(0, fi.fFilename);
	    //fdatafile->Layout();
	    
}


void GoGamecock::GoProgress(int position)
{
   
   fHProgress->SetPosition(position);
   //fHProgress->Reset();
   //int cnt = 0;
   //int inc = 4;
   
//    {
//    	if (cnt < 100) {
//          //printf("Go %s \n", cnt);
// 	 cnt += inc;
//          fHProgress->Increment(inc);
// 	 //gSystem->Sleep(100);
//    	}
//    }
   //gSystem->ProcessEvents();
}


void GoGamecock::GetMakeBatch(const char* command) {
  //gSystem->Exec("rm -rf /tmp/gamecock.log");
  //gSystem->Exec("touch /tmp/gamecock.log");
  ofstream of((const char*) extShellScript);
  of << "#!/bin/csh" << endl;
  of << command << " < " << extInputPipe << " >>& " << extOutputPipe << " & " << endl;
  //of << command << " >&  /tmp/gamecock.log & " << endl;
}

void GoGamecock::get()
{
  if (WhichButton1 < 0) {
    new TGMsgBox(fClient->GetRoot(), this, "create histogram error", 
		 "nothing selected", kMBIconStop);
    return;
  }
  
  const char *datafile=fdatafile_create->GetText();
  if (!strlen(datafile)) {
    new TGMsgBox(fClient->GetRoot(), this, "create histogram error", 
		 "no input file", kMBIconStop);
    return;
  }
  
  SwitchPicture(0);
  gSystem->ExpandPathName(extShellScript);
  gSystem->ExpandPathName(extOutputPipe);
  gSystem->ExpandPathName(extInputPipe);
  
  if (gCalib) delete gCalib;
  gCalib = new Calibration (selCalib[WhichButton1], selItem[WhichButton1]); 
  
  int retval;
  
  // recreate pipe, so we are sure that it is empty on startup
  retval = gSystem->Exec(TString("rm -rf ") + extOutputPipe);                // todo: check for error
  retval = gSystem->Exec(TString("mknod ") + extOutputPipe + TString(" p")); 
  retval = gSystem->Exec(TString("rm -rf ") + extInputPipe);   
  retval = gSystem->Exec(TString("mknod ") + extInputPipe + TString(" p"));

  TString extCommand(Execute_external_command[WhichButton1]);
  gSystem->ExpandPathName(extCommand);
  
  TString launchCommand = 
    TString(Execute_external_command[WhichButton1]) + TString(" ") +
    TString(Execute_external_option[WhichButton1])  + TString(" ") +
    datafile;

  TString logMsg = TString("=== start <") + launchCommand + TString("> ====");
  GetMakeBatch (launchCommand);

  // make shell script executable
  TString makeExecutable = TString("chmod 755 ") + extShellScript;
  retval = gSystem->Exec(makeExecutable);

  gCock->AddLog((const char*)logMsg);

  TString launchProgram = extShellScript + TString(" & ");
  // start the external program by executing the shell script
  retval =  gSystem->Exec(launchProgram);

  /*  FILE* frootcom =*/ fopen((const char*) extInputPipe, "w");

  // open pipe to access output of externel program
  FILE* f = fopen((const char*) extOutputPipe, "r");
  if (!f) {
    throw (const char*) (TString("can't open pipe ") + extOutputPipe);
  }
  rp = new ReadPipe(f); 

}

void GoGamecock::fit()
{
  if (WhichButton2 < 0) {
    new TGMsgBox(fClient->GetRoot(), this, "fit histogram error", 
		 "nothing selected", kMBIconStop);
    return;
  }
  SwitchPicture(0);
  fEdit->Clear();
  if (gCalib) delete gCalib;
  gCalib = new Calibration (selCalib[WhichButton2], selItem[WhichButton2]); 

  string histgfile(fdatafile_fit->GetText());
  int len = histgfile.length();
  if (len) {
    gCalib->SetHistFilename(histgfile);

    if (!gCalib->IsRootHist()) {
      gCock->AddLog("try to convert hbook histogram file...");
      string rootfilename;
      if (histgfile.substr(len-6,6)==".hbook")
	rootfilename = histgfile.substr(0,len-6) + ".root";
      else if (histgfile.substr(len-4,4)==".hst")
	rootfilename = histgfile.substr(0,len-4) + ".root";
      else if (histgfile.substr(len-3,3)==".rz")
	rootfilename = histgfile.substr(0,len-3) + ".root";
      else
	rootfilename = histgfile + ".root";
      TString h2root ("$ROOTSYS/bin/h2root");
      gSystem->ExpandPathName(h2root);
      string command(h2root.Data());
      command += string(" ") + histgfile + " " + rootfilename;
      int retval = gSystem->Exec(command.c_str());
      if (retval) {
	new TGMsgBox(fClient->GetRoot(), this, "fit histogram error", 
		 "h2root not installed or unable to convert hbook file", kMBIconStop);
	return;
      }
      gCock->AddLog("done!");
      gCalib->SetHistFilename(rootfilename);
    }
  }

  showall();
}


SettingsWindow::SettingsWindow(const TGWindow *p, const TGWindow *main, UInt_t w,
                       UInt_t h, UInt_t options)
    : TGTransientFrame(p, main, w, h, options),whichtab(0)
{
   // Create a dialog window. A dialog window pops up with respect to its
   // "main" window.

   // Used to store GUI elements that need to be deleted in the ctor.
   fCleanup = new TList;

   fFrame1 = new TGHorizontalFrame(this, 60, 20, kFixedWidth);

   fOkButton = new TGTextButton(fFrame1, "&Ok", 1);
   fOkButton->Associate(this);
   fCancelButton = new TGTextButton(fFrame1, "&Cancel", 2);
   fCancelButton->Associate(this);

   fL1 = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX,
                           2, 2, 2, 2);
   fL2 = new TGLayoutHints(kLHintsBottom | kLHintsRight, 2, 2, 5, 1);

   fFrame1->AddFrame(fOkButton, fL1);
   fFrame1->AddFrame(fCancelButton, fL1);

   fFrame1->Resize(150, fOkButton->GetDefaultHeight());
   AddFrame(fFrame1, fL2);

   //--------- create Tab widget and some composite frames for Tab testing

   fTab = new TGTab(this, 300, 300);
   fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft, 5, 5, 5, 5);
   TGCompositeFrame *tf;
   
   //red color
   /*
   Pixel_t red;
   fClient->GetColorByName("red", red);
   fRedTextGC.SetForeground(red);
   */
    
    char textline[80];
    ifstream datainput("gamecock.ini");

    const char *TabName[7] = { "atten", "gmean", "mass", "p2pdelay", "veff", "tdc","timewalk"};
   
   for (int i = 0; i < 7; i++) {
   
      tf = fTab->AddTab(TabName[i]);
      tf->SetLayoutManager(new TGVerticalLayout(tf));
      char buff[100];
            
      //Create setting box
      fF_Create[i] = new TGGroupFrame(tf, "Create Histogram options", kVerticalFrame);
      //fF1->SetTitlePos(TGGroupFrame::kRight);   // right aligned
      tf->AddFrame(fF_Create[i], fL3);
   
      // 3 column, n rows
      fF_Create[i]->SetLayoutManager(new TGMatrixLayout(fF_Create[i], 0, 2, 10));
      //fF_Create[i]->SetLayoutManager(new TGTableLayout(fF_Create[i], 0, 2));
      const char *LabelName_Create[4] = {"Program path and name:", "Command line options: ", "Output histogram:     ", "Runindex:             "};
      
      //get rid of name line like "[atten]"
      datainput.getline(textline,79); 
      
      for (int j = 0; j < 4; j++) {
         sprintf(buff, "%s", LabelName_Create[j]);   	      
         fF_Create[i]->AddFrame(new TGLabel(fF_Create[i], new TGHotString(buff)));
         fF_Create[i]->AddFrame(new TGLabel(fF_Create[i], new TGHotString("")));
   
         datainput.getline(textline,79); //read the parameter line
           // remove trailing end of line
		if (strlen(textline)) {
    			char* last = textline + strlen(textline) - 1;
    			if (*last == '\n') *last = 0;
  		}         
          printf("%s\n",textline);
          
         tbuf_create[i][j] = new TGTextBuffer(100);         
         tbuf_create[i][j]->AddText(0, textline);
         
         tent_create[i][j] = new TGTextEntry(fF_Create[i], tbuf_create[i][j]);
         tent_create[i][j]->Associate(this);
         tent_create[i][j]->Resize(500, tent_create[i][j]->GetDefaultHeight());
         tent_create[i][j]->DrawBorder();
         tent_create[i][j]->SetFont("-adobe-courier-bold-r-*-*-14-*-*-*-*-*-iso8859-1");
         fCleanup->Add(tent_create[i][j]);
         //fF_Create[i]->AddFrame(tent_create[i][j],new TGLayoutHints(kLHintsExpandX | kLHintsLeft ,5,5,5,5));
         fF_Create[i]->AddFrame(tent_create[i][j],new TGLayoutHints(kLHintsLeft));
         //add buttons to open file
         switch (j){
         	case 0:
	           fProgram_location = new TGPictureButton(fF_Create[i], fClient->GetPicture("open.xpm"),id_program_location);
	           fProgram_location->Associate(this);
	 	   fF_Create[i]->AddFrame(fProgram_location, new TGLayoutHints(kLHintsCenterY | kLHintsRight,30,5,5,5));
	 	   break;
	 	case 1:
	           fF_Create[i]->AddFrame(new TGLabel(fF_Create[i], new TGHotString("")));
	 	   break;
	 	case 2:
	           fOutput_location = new TGPictureButton(fF_Create[i], fClient->GetPicture("open.xpm"),id_output_location);
	           fOutput_location->Associate(this);
	 	   fF_Create[i]->AddFrame(fOutput_location, new TGLayoutHints(kLHintsCenterY | kLHintsRight,30,5,5,5));
	 	   break;
	 	case 3:
	           fF_Create[i]->AddFrame(new TGLabel(fF_Create[i], new TGHotString("")));
	 	   break;
	 	default:
         	   break;
         }	 	 	 	   
      }      
      fF_Create[i]->Resize(); // resize to default size
      
      //Fit settings box
      fF_Fit[i] = new TGGroupFrame(tf, "Fit Histogram options", kVerticalFrame);
      //fF1->SetTitlePos(TGGroupFrame::kRight);   // right aligned
      tf->AddFrame(fF_Fit[i], fL3);
   
      // 6 column,  n rows
      fF_Fit[i]->SetLayoutManager(new TGMatrixLayout(fF_Fit[i], 0, 6, 10));
      
      //Add the paprameter labels
      const char *LabelName_Fit[6] = {"", "Default", "Slider min", "max", "Fit min", "max"};
      for (int j = 0; j < 6; j++) {
         sprintf(buff, "%s", LabelName_Fit[j]);   	      
         fF_Fit[i]->AddFrame(new TGLabel(fF_Fit[i], new TGHotString(buff)));
      }
      
      //add the parameters
      for (int j = 0; j < 6; j++) {
         sprintf(buff, "Parameter %i", j);   	      
         fF_Fit[i]->AddFrame(new TGLabel(fF_Fit[i], new TGHotString(buff)));
   
   	 datainput.getline(textline,79); //read the line "DefaultPar;Slider LowerLimit;Slider UpperLimit;Fit min;Fit max"
   	 
         for (int k = 0; k < 5; k++) {
         	
           datainput >> textline; //read parameters
                            
            tbuf_fit[i][j][k] = new TGTextBuffer(100);
            tbuf_fit[i][j][k]->AddText(0, textline);
            
            tent_fit[i][j][k] = new TGTextEntry(fF_Fit[i], tbuf_fit[i][j][k]);
            tent_fit[i][j][k]->Associate(this);
            tent_fit[i][j][k]->Resize(100, tent_fit[i][j][k]->GetDefaultHeight());
            tent_fit[i][j][k]->SetFont("-adobe-courier-bold-r-*-*-14-*-*-*-*-*-iso8859-1");
            //fCleanup->Add(tent_fit[i][j][k]);
            fF_Fit[i]->AddFrame(tent_fit[i][j][k]);
        }
        // remove trailing end of line
	//datainput.getline(textline,79);	
      }      
      fF_Fit[i]->Resize(); // resize to default size      
      
      datainput.getline(textline,79);
      //get rid of a blank line
      datainput.getline(textline,79);

   }

   
     // make the first active tab yellow
     Pixel_t yellow;
     fClient->GetColorByName("yellow", yellow);
     TGTabElement *tt;
     tt = fTab->GetTabTab(whichtab);
     tt->ChangeBackground(yellow); 
     
   //--- end of last tab

   fL4 = new TGLayoutHints(kLHintsBottom | kLHintsExpandX |
                                          kLHintsExpandY, 2, 2, 5, 1);
   AddFrame(fTab, fL4);

   MapSubwindows();
   Resize(700,620);   // resize to default size

   // position relative to the parent's window
   //CenterOnParent();

   SetWindowName("Settings");

   MapWindow();
   //fClient->WaitFor(this);    // otherwise canvas contextmenu does not work
}

SettingsWindow::~SettingsWindow()
{
   // Delete dialog widgets.

   /*fCleanup->Delete();
   delete fCleanup;
   delete fOkButton;
   delete fCancelButton;
   delete fFrame1;
   delete fF_Create[6]; delete fF_Fit[6];
   delete fTab;
   delete fL3; delete fL4;
   delete fL1; delete fL2;   */
}

void SettingsWindow::CloseWindow()
{
   // Called when window is closed via the window manager.

#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif

}

Bool_t SettingsWindow::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
   // Process messages coming from widgets associated with the dialog.

   switch (GET_MSG(msg)) {
      case kC_COMMAND:

         switch (GET_SUBMSG(msg)) {
            case kCM_BUTTON:
              switch(parm1) {
                  case 1:
                     CloseWindow();
                     break;
                  case 2: 
                     printf("\nTerminating Settings: %s pressed\n",(parm1 == 1) ? "OK" : "Cancel");
                     CloseWindow();
                     break;
                  case id_program_location:
                     Open_program_location();
                     break;
   		  case id_output_location:
   		     Open_output_location();
   		     break;
                  default:
                     break;
               }                
               break;
            case kCM_TAB:
               // make inactive tab gray
               Pixel_t gray;
               fClient->GetColorByName("gray", gray);
               TGTabElement *t;
               t = fTab->GetTabTab(whichtab);
               t->ChangeBackground(gray);
            
               whichtab=parm1;   
               printf("Tab item %ld activated\n", parm1);
                           
               // make active tab yellow
               Pixel_t yellow;
               fClient->GetColorByName("yellow", yellow);
               TGTabElement *tt;
               tt = fTab->GetTabTab(whichtab);
               tt->ChangeBackground(yellow);
               break;
            default:
               break;
         }
         break;

      default:
         break;
   }
   return kTRUE;
}

void SettingsWindow::Open_program_location()
{
            static TString dir(".");
            TGFileInfo fi;
            fi.fFileTypes = filetypes_create;
            fi.fIniDir    = StrDup(dir.Data());
            printf("fIniDir = %s\n", fi.fIniDir);
            new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fi);
            //printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
            dir = fi.fIniDir;
	    //datafile = fi.fFilename;
	    printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
	    tbuf_create[whichtab][0]->Clear();
	    if (fi.fFilename)
	      tent_create[whichtab][0]->SetText(fi.fFilename);
	    //tbuf_create[0]->AddText(0, fi.fFilename);
	    //tent_create[0]->Layout();
	    
}

void SettingsWindow::Open_output_location()
{
            static TString dir(".");
            TGFileInfo fi;
            fi.fFileTypes = filetypes_create;
            fi.fIniDir    = StrDup(dir.Data());
            printf("fIniDir = %s\n", fi.fIniDir);
            new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fi);
            //printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
            dir = fi.fIniDir;
	    //datafile = fi.fFilename;
	    printf("Open file: %s (dir: %s)\n", fi.fFilename, fi.fIniDir);
	    tbuf_create[whichtab][2]->Clear();
	    if (fi.fFilename)
	      tent_create[whichtab][2]->SetText(fi.fFilename);
	    //fTbdatafile->AddText(0, fi.fFilename);
	    //fdatafile->Layout();
	    
}

AboutWindow::AboutWindow(const TGWindow *main, UInt_t w, UInt_t h) :
    TGTransientFrame(gClient->GetRoot(), main, w, h)
{
   // Create an editor in a dialog.
   fPic = gClient->GetPicture(GamecockResource("gamecock.xpm"));
   fImageMap = new TGImageMap(this,fPic);
   AddFrame(fImageMap,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,5,5,5,5));
       
    SetWindowName("Gamecock");
    SetIconName("Gamecock");
 
    ok = new TGPictureButton(this,fClient->GetPicture(GamecockResource("ok.xpm")));
    AddFrame(ok, new TGLayoutHints(kLHintsCenterX,5,5,5,5));
    
   // position relative to the parent's window
   Window_t wdum;
   int ax, ay;
   // editor covers right half of parent window
   gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
                        0,
                        (Int_t)(((TGFrame *) main)->GetHeight() - fHeight) >> 1,
                        ax, ay, wdum);

   Move((((TGFrame *) main)->GetWidth() >> 1) + ax, ay);

   SetWMPosition((((TGFrame *) main)->GetWidth() >> 1) + ax, ay);
 
   MapSubwindows();
   Resize(GetDefaultSize());  
   
   MapWindow();
}

AboutWindow::~AboutWindow()
{
   delete fImageMap;
}

void AboutWindow::CloseWindow() {
#ifdef USE_OLD_ROOTVERSION
  delete this;
#else
  DeleteWindow();
#endif
}

Bool_t AboutWindow::ProcessMessage(Long_t msg, Long_t parm1, Long_t index)
{
   switch (GET_MSG(msg)) {
      case kC_COMMAND:
         switch (GET_SUBMSG(msg)) {
            case kCM_BUTTON:
               // Only one button and one action...
               CloseWindow();
               break;
            default:
               break;
         }
         break;
      default:
         break;
   }
   return kTRUE;
}

//---- Main program ------------------------------------------------------------
//#ifdef STANDALONE
//TROOT root("GUI", "GUI test environement");



int main(int argc, char **argv)
{
  TApplication theApp("App", &argc, argv);

  if (gROOT->IsBatch()) {
    fprintf(stderr, "%s: cannot run in batch mode\n", argv[0]);
    return 1;
  }
  
  try {
    gCock = new GoGamecock(gClient->GetRoot(), 1000, 610);  
    theApp.Run();
  }
  catch (const char* e) {
    cerr << "Exception: " << e << endl;
    exit (-1);
  }
  catch (...) {
    cerr << "Got an unknown exception" << endl;
    exit (-1);
  }

  return 0;
}
//#endif

