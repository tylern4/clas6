#ifndef __GAMECOCK_H
#define __GAMECOCK_H
#include "ROOT.h"
#include "ReadPipe.h"
#include "Calibration.h"

using namespace std;

const int MAX_RADIO_create = 7;
const int MAX_RADIO_fit = 13;

class GoGamecock;

class Startanim : public TObject {
  int count;
  TTimer* timer;
  GoGamecock* super;
public:
  Startanim(GoGamecock* super_);
  void TurnOn();
  Bool_t HandleTimer(TTimer* timer_);
};
  

class GoGamecock : public TGMainFrame {

//RQ_OBJECT("GoGamecock")

private:
  int   WhichButton1,WhichButton2;

   TGMenuBar          *fMenuBar;
   TGPopupMenu        *fMenuFile, *fMenuSettings,*fMenuHelp;
   
   TGLayoutHints      *fMenuBarLayout, *fMenuBarItemLayout, *fMenuBarHelpLayout;
   TRootEmbeddedCanvas *fEcanvas;			
   
   TGHorizontalFrame   *hframe, *hframe_create_file, *hframe_fit_file;
   TGVerticalFrame     *vframe_left, *vframe_right, *vframe_create_file_label, *vframe_fit_file_label;
   TGGroupFrame      *bg_create, *bg_fit, *vframe_create, *vframe_fit;
   TGVerticalFrame     *smallbox_create[3], *smallbox_fit[3];

   TGTextView       	*fEdit;   // text edit widget
   TGHProgressBar        *fHProgress;
   
   TGLabel 		*lab_create, *lab_fit;
   TGTextEntry		*fdatafile_create, *fdatafile_fit;
   TGTextBuffer 		*fTbdatafile_create, *fTbdatafile_fit;
   TGPictureButton  	*data_create, *data_fit, *go_create, *go_fit, *exit;
   TGRadioButton       	*fRget[MAX_RADIO_create],*fRfit[MAX_RADIO_fit];   
   
   TGLayoutHints     	*fL_expandXY, *fL_expandX, *fL_left_expandX; // layout of TGTextEdit, hframe and vframe
   TGLayoutHints       	*fL_left_expandY_centerY;    //layout of buttons
   const TGPicture*        fPic[10];
   TGImageMap*         fImageMap[10]; 
     
  bool islog;
  ReadPipe* rp; 
  
public:
  GoGamecock(const TGWindow *p, UInt_t w, UInt_t h);
  virtual ~GoGamecock();
  
  void CloseWindow();
  void HandleMenu(Int_t id);
  void HandlePopup() { printf("menu popped up\n"); }
  void HandlePopdown() { printf("menu popped down\n"); }
  void SwitchPicture(int ipic);
  void   SetTitle();
  void   AddLog(const char* logline);

  void   OpenData_create();
  void   OpenData_fit();
  void   get();
  void   fit();
  void   GoProgress(int position);
  void   GetMakeBatch(const char* command);
  
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t index);
};

class SettingsWindow : public TGTransientFrame {

private:

   TGCompositeFrame    *fFrame1;
   TGGroupFrame        *fF_Create[7], *fF_Fit[7];
   TGButton            *fOkButton, *fCancelButton;
   TGTab               *fTab;
   TGLayoutHints       *fL1, *fL2, *fL3, *fL4;
   TList               *fCleanup;
   TGPictureButton     *fProgram_location, *fOutput_location;
   TGTextEntry         *tent_create[7][4], *tent_fit[7][6][5];
   TGTextBuffer 	*tbuf_create[7][4], *tbuf_fit[7][6][5];
   TGGC                  fRedTextGC;
   
   int whichtab;


public:
   SettingsWindow(const TGWindow *p, const TGWindow *main, UInt_t w, UInt_t h,
               UInt_t options = kVerticalFrame);
   virtual ~SettingsWindow();

   void   Open_program_location();
   void   Open_output_location();
   
   virtual void CloseWindow();
   virtual Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
};


class AboutWindow : public TGTransientFrame {

private:
  TList* widgets;
      
  const TGPicture*  fPic;       // displayed picture
  TGImageMap*       fImageMap; 
  TGPictureButton   *ok;
  
public:
  AboutWindow(const TGWindow *main, UInt_t w, UInt_t h);
  virtual ~AboutWindow();
   
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
  void CloseWindow();
};
#endif
