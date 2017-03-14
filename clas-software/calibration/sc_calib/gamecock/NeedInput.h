#ifndef __NEEDINPUT_H
#define __NEEDINPUT_H

#include <ctype.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include "Calibration.h"
#include "ButtonOkCancel.h"
#include "ReadRoc.h"
#include "BankDescription.h"

const int id_needOK      = 5555;
const int id_needEntry   = 70;
const int id_needPic     = 80;

class NeedConstants {
  double* Yoffset[2];
  ReadRoc* rroc;
public:
  NeedConstants(BankDescription* bd);
  NeedConstants(const char* yoffname, const char* yerrname);
  ~NeedConstants();
  ReadRoc* GetRoc()          { return rroc; };
  double*  GetYoffset(int i) { return Yoffset[i]; };
};

class NeedInput : public TGTransientFrame {
  TList*            widgets;
  TGWindow*         parent;
  int               numberFiles;
  TGLabel*          lab[5];
  TGTextEntry*      ent[5];
  TGPictureButton*  pic[5];
public:
  NeedInput(const TGWindow *p, TGWindow *parent_, UInt_t w);
  ~NeedInput();
  Bool_t ProcessMessage(Long_t msg, Long_t parm1, Long_t so);
  bool SelectionOK();
  void SelectFile(int ient);
  void CloseWindow();
};

#endif
