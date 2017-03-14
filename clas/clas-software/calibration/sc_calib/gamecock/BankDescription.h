#ifndef __BANKDESCRIPTION_H
#define __BANKDESCRIPTION_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <ctype.h>

class BankDescription {
  static int maxDescr;
  int icode;
  std::string bank;
  int maxinx;
  int minsec;
  int maxsec;
  int maxstr;
  int maxlay;
  int type;
  int cols;
  int leftright;
  int layer2lr;
  int minlay;
  int minstr;
public:
  BankDescription ();
  BankDescription (const std::string sdesc);
  std::string GetBankName() { return bank;} 
  int GetCode()             { return icode; }
  int GetMaxDescription()   { return maxDescr; } 
  int GetMaxIndex()         { return maxinx; }
  int GetMaxStripe()        { return maxstr; }
  int GetMaxLayer()         { return maxlay; }
  int GetMaxSector()        { return maxsec; }
  int GetMinSector()        { return minsec; }
  int GetModuleType()       { return type; }
  int GetColumns()          { return cols; }
  int GetMinLayer()         { return minlay; }
  int GetMinStripe()        { return minstr; }
  bool IsLeftRight()        { return leftright != 0; }
  bool IsLayer2LR()         { return layer2lr  != 0; }
  bool IsPipeline()         { return type == 1190; }
  int CalculateIndex(int sec, int id, int lr=0);
  const std::string GetHistogramName(int index);
  friend std::istream& operator>> (std::istream&, 
				   BankDescription&); 
};

void read_tdc_calib_banks();
#endif 
