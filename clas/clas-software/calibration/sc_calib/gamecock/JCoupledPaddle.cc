#include <iostream>
#include <stdlib.h>

extern "C" {
#include "ntypes.h"
#include "bostypes.h"
#include "scExtern.h"
  int coupled_paddle_index (int sector, int stripe, int itbtr);
  int coupled_paddle_z2index (int sector, int stripe, double zpos);
}

using namespace std;

class JBosBank {
  /// virtual class, all banks derive from this one
protected:
  bool found;
  bankHeader_t* head;
public:
  JBosBank () : found(false), head(NULL) {};
  bool Found()   { return found; }
  int  GetNrows() { return found ? head->nrow : 0 ; }
  int  GetNcols() { return found ? head->ncol : 0 ; }
  void CheckIndex(int i) {
    if (!found) throw -1;
    if (i < 0) throw -2;
    if (i >= GetNrows()) throw -3;
  }
};

class Jscg : public JBosBank {
  clasSCG_t* p;
public:
  Jscg (int i) : JBosBank() {
    p = (clasSCG_t*) getGroup(&wcs_, "SCG ", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  scg_t& operator[] (int i) {
    CheckIndex(i);
    return p->scg[i];
  }
};

class Jtdpl : public JBosBank {
  clasTDPL_t* p;
public:
  Jtdpl (int i) : JBosBank() {
    p = (clasTDPL_t*) getGroup(&bcs_, "TDPL", i);
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tdpl_t& operator[] (int i) {
    CheckIndex(i);
    return p->tdpl[i];
  }
};

class Jtbtr : public JBosBank {
  clasTBTR_t* p;
public:
  Jtbtr () : JBosBank() {
    p = (clasTBTR_t*) getBank(&bcs_, "TBTR");
    if (p) {
      found = true;
      head = &(p->bank);
    }
  }
  tbtr_t& operator[] (int i) {
    CheckIndex(i);
    return p->tbtr[i];
  }
};


bool firsttime = true;
int scg_not_found() {
  if (firsttime) {
    cout << "SC geometry not intialized" << endl;
    firsttime = false;
  }
  return -1;
}

double z_unknown() {
  return -9999.;
}

/// returns   stripe_1..39=0   A=1   B=2   or negative number for errors 
//unless SC_VERSION_FLAG == 2, then returns 0.
int coupled_paddle_index (int sector, int stripe, int itbtr) {
  if ((stripe < 40) || (SC_VERSION_FLAG == 2))
    return 0;
  Jscg scg(sector);
  if (!scg.Found())
    return scg_not_found();

  /// get tbtr bank
  Jtbtr tbtr;
  if (!tbtr.Found() || itbtr < 0 || itbtr >= tbtr.GetNrows()) 
    return -2;

  /// get tdpl bank for sector
  int itbtl = (tbtr[itbtr].itr_sec % 100 - 1) * 10; /// 10 planes per track
  int iplan = scg[stripe-1].panel + 3;              /// SC plane 4, 5, 6, 7
  itbtl += iplan;
  Jtdpl tdpl(sector);

  if (!tdpl.Found() || itbtl < 0 || itbtl >= tdpl.GetNrows()) 
    return -3;

  double zHitpos = tdpl [itbtl].pos.z;
  double zBorder = (scg[stripe-1].zcw +  scg[stripe-1].zccw) / 2.;

  if (zHitpos < zBorder) 
    return 2;

  return 1;
}

/// returns   stripe_1..39=0   A=1   B=2   or negative number for errors 
//unless SC_VERSION_FLAG == 2, then returns 0.
int coupled_paddle_z2index (int sector, int stripe, double zpos) {
  if ((stripe < 40) || (SC_VERSION_FLAG == 2))
    return 0;
  Jscg scg(sector);
  if (!scg.Found())
    return scg_not_found();

  double zBorder = (scg[stripe-1].zcw +  scg[stripe-1].zccw) / 2.;

  if (zpos < zBorder) 
    return 2;

  return 1;
}

// z-hit pos. relative to center of stripe
double z_relative(int sector, int stripe, int itbtr) {
  Jscg scg(sector);
  if (!scg.Found()) return z_unknown();

  /// get tbtr bank
  Jtbtr tbtr;
  if (!tbtr.Found() || itbtr < 0 || itbtr >= tbtr.GetNrows()) 
    return z_unknown();

  /// get tdpl bank for sector
  int itbtl = (tbtr[itbtr].itr_sec % 100 - 1) * 10; /// 10 planes per track
  int iplan = scg[stripe-1].panel + 3;              /// SC plane 4, 5, 6, 7
  itbtl += iplan;
  Jtdpl tdpl(sector);

  if (!tdpl.Found() || itbtl < 0 || itbtl >= tdpl.GetNrows()) 
    return z_unknown();

  double zHitpos = tdpl [itbtl].pos.z;
  double zBorder = (scg[stripe-1].zcw +  scg[stripe-1].zccw) / 2.;

  return zHitpos - zBorder; 
}
