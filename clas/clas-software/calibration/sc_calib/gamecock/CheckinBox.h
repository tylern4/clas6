#ifndef __CHECKINBOX_H
#define __CHECKINBOX_H

#include <ctype.h>
#include <stdlib.h>
#include <fstream>
#include "Calibration.h"
#include "SConstants.h"
#include "CaldbServer.h"
#include "ButtonOkCancel.h"
#include "CheckinValues.h"

#ifndef MAX_TABLE
#define MAX_TABLE 8
#endif
const int idOnlyF   = 10;
const int idFile    = 11;
const int idFileSel = 12;
const int idPathSel = 13;
const int idComCT   = 18;
const int idComCI   = 19;

const int idOptBut  = 30;
const int idParams  = 40;
const int idMysql   = 50;
const int idReserved= 60;

enum dbaseCreate_t { idHostname, idUser, idPasswd, idRunIndex, idMin, 
		 idSrmin, idComment, idPath, idMax, idSrmax };

class CheckinSettings;

class ItemDetails : public TGTransientFrame {
  CheckinSettings* cs;
  TGVerticalFrame* fl;
  TGVerticalFrame* fe;
  TGCheckButton*   onlyf;
  TGTextEntry*     efile;
  TGTextEntry*     entCT;
  TGTextEntry*     entCI;
  TGLabel*         labCT;
  TGLabel*         labCI;
  TGPictureButton* bfile;
public:
  ItemDetails (const TGWindow* p, CheckinSettings* cs_);
  ~ItemDetails ();
  void DisableCaldbFields(bool disable);
  void SelectFile();
  void AcceptSettings();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t index);
  void CloseWindow();
};

class CheckinBox : public TGTransientFrame {
  TList* widgets;
  bool busy;
  int n_table;
  CheckinValues*     val[MAX_TABLE];
  CheckinSettings*   checkins[MAX_TABLE];    
  TGHorizontalFrame* fpar[MAX_TABLE];
  TGCheckButton*     cpar[MAX_TABLE];
  TGTextButton*      dpar[MAX_TABLE];
  TGTextEntry*       ent [10];
  TGLabel*           status;
  bool IsNumeric(int ifield);
  void Srmin2Srmax();
public:
  CheckinBox(const TGWindow *p, UInt_t w);
  ~CheckinBox();
  int PrepareCheckin();
  int GetNtable() {return n_table;}
  bool CheckinDone();
  void SelectPath();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t so);
  void CloseWindow();
  friend class CheckinSettings;
};

class CheckinSettings {
  int  ifield;    /// field in user interface
  int  ipar;      /// number of parameters to save
  int  iserr;     /// uncertainty = 1, value = 0
  char subsystem[40];
  char item[40];
  char directory[80];
  char filename[80];
  char hostname[80];
  char user[80];
  char passwd[80];
  char runindex[80];
  int  min;
  int  max;
  int  srmin;
  int  srmax;
  char commentTable[256];
  char commentIndex[256];
public:
  bool disregard;
  bool changedFile;   /// altered manually?
  bool changedCT;
  bool changedCI;
  bool fileonly;  /// don't write in database
  bool nocaldb;   /// no table existing in base
  CheckinSettings (int ifield_);
  CheckinSettings (const CheckinSettings& cs);
  void SetSettings(const CheckinBox* cb);
  int GetMinRun() {return min; }
  int GetMaxRun() {return max; }
  int GetSrmin()  {return srmin; }
  int GetSrmax()  {return srmax; }
  const char* GetHostname()     {return hostname;  }
  const char* GetUsername()     {return user;      }
  const char* GetPassword()     {return passwd;    }
  const char* GetRunIndex()     {return runindex;  } 
  const char* GetCommentTable() {return commentTable; };
  const char* GetCommentIndex() {return commentIndex; };
  const char* GetFilename()     {return filename;  }
  const char* GetDirectory()    {return directory; } 
  const char* GetSubsystem()    {return subsystem; }
  const char* GetItem()         {return item;      }
  const char* ValueError()      {return ((iserr)?"uncertainty":"value"); }
  void SetCommentTable(const char* s) {strcpy(commentTable,s); };
  void SetCommentIndex(const char* s) {strcpy(commentIndex,s); };
  void SetFilename(const char* s)     {strcpy(filename,s); }
  void DefaultFilename(char* s);
  void DefaultCommentIndex(char *s, const char* comment);
};

#endif
