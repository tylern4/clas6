#ifndef __CALDBBOX_H
#define __CALDBBOX_H

#include <ctype.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include "jdefine.h"
#include "MySQL.h"
#include "CaldbServer.h"
#include "Calibration.h"
#include "ButtonOkCancel.h"
#include "SConstants.h"

using namespace std;

#ifndef MAX_TABLE
#define MAX_TABLE 8
#endif
const int MAX_ROW = 16;

const int id_Caldb  = 110;
const int id_SelBut = 120;
// const int id_CaldbSelOk = 5334;

const int caldbbox_w = 700;
enum dbaseGet_t { id_Hostname, id_User, id_Passwd, id_RunIndex, id_RunNo };

class CaldbBox;
class CaldbHistory;

class CaldbTabSel {
  bool selectionDone;
  int  itemValueId;
  int  runno;
  string hostname;
  string user;
  string passwd;
  string system;
  string subsystem;
  string item;
  string runindex;
  string date;
  string author;
  string comment;

public:
  CaldbTabSel (const char* sys, const char* subsys, const char* it );
  void SetSettings(const CaldbBox* cbox);
  void SetRunno(int r) { runno = r; }
  void SetSystem(string &locSystem) { system = locSystem; }
  void SetSystem(const char* locSystem) { system = locSystem; }
  int GetRunno(){return runno;}
  bool CommonSettings(const CaldbBox* cbox);
  bool GetValues(double* values);
  bool SelectTable(CaldbBox* p);
  bool IsSelectionDone() { return selectionDone; }
  TGString* GetLabel();
  TGString* GetItem();
  friend class CaldbHistory;
};

class CaldbBox : public TGTransientFrame {
  static string sRunIndex;
  TList*             widgets;
  bool busy;
  int n_table;
  SConstants*        gotcha;
  TGLabel*           labtab[MAX_TABLE];
  CaldbTabSel*       seltab[MAX_TABLE];
  TGLabel*           status;
  TGTextEntry*       ent[4];
  TGNumberEntry*     runno;
public:
  CaldbBox(const TGWindow *p, SConstants* gotcha_);
  ~CaldbBox();
  int GetNtable() {return n_table;}
  void Label4Table();
  bool SelectTable(int itab);
  bool GetValues();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t so);
  void CloseWindow();
  friend class CaldbTabSel;
};

class CaldbHistory: public TGTransientFrame {
  const TGWindow* parent;
  TList*    widgets;
  CaldbTabSel* seltab;
  int nrow;
  TGLabel* lab[MAX_ROW] [8];
  const char* GetField(TSQLRow* row, int ifield);
public:
  CaldbHistory(const TGWindow *p, const TGWindow *parent_, 
	       CaldbTabSel* seltab_, TSQLResult* res);
  ~CaldbHistory();
  void SelectionOK(int index);
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t index);
  void CloseWindow();
};

#endif
