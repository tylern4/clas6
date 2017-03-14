#include "BankDescription.h"
#include <sstream>
#include <string>
#include <cstdlib>

using namespace std;
extern int SC_Version_Flag;

BankDescription::BankDescription() : icode(maxDescr++),
  maxinx(0),  minsec(0),  maxsec(0),  maxstr(0),  maxlay(0),
  type(0),  cols(0),  leftright(0), layer2lr(0), minlay(0),  minstr(0) 
{}

istream& operator>> (istream& is, BankDescription& bd) {
  string options;
  unsigned int equalsign;
  is >> bd.bank >> bd.maxinx >> bd.minsec >> bd.maxsec 
     >> bd.minstr >> bd.maxstr >> bd.maxlay 
     >> bd.type >> bd.cols >> options;

  if ((equalsign = options.find('=')) != string::npos) {
    string keyword = options.substr(0,equalsign);
    string value = options.substr(equalsign+1);
    if      (keyword == "minlayer")  bd.minlay    = atoi(value.c_str());
    else if (keyword == "leftright") bd.leftright = atoi(value.c_str());
    else if (keyword == "layer2lr")  bd.layer2lr  = atoi(value.c_str());
    else cerr << "unknown keyword <" << keyword << ">" << endl;
  }
  return is;
} 

BankDescription::BankDescription(const string sdesc) :icode(maxDescr++),
  maxinx(0),  minsec(0),  maxsec(0),  maxstr(0),  maxlay(0),
  type(0),  cols(0),  leftright(0), layer2lr(0), minlay(0),  minstr(0) 
{
  istringstream iss(sdesc);
  iss >> (*this);
}


int BankDescription::CalculateIndex(int sec, int id, int lr) {
  int multsec = maxsec ? maxsec : 1;
  int multlay = maxlay ? maxlay : 1;
  int layer = 0;
  if (maxlay) 
    layer = id & 0x100;
  id = id&0xFF;
  if (layer2lr && layer) {
    lr = 1;
    layer = 0;
  }

   int index =     lr     * multsec * multlay * maxstr
    + (sec - minsec) * multlay * maxstr
    + (layer - minlay)         * maxstr
    + id - minstr;

  return index;
}

const string BankDescription::GetHistogramName(int index) {
  ostringstream hstname;
  int multsec = maxsec ? maxsec : 1;
  int multlay = maxlay ? maxlay : 1;
  int lr = index / (multsec * multlay * maxstr);
  index %= multsec * multlay * maxstr;
  int sec = index / (multlay * maxstr);
  index %= multlay * maxstr;
  int lay = index / maxstr;
  index %= maxstr;
  int str=index+minstr;
  
  hstname << bank << "_";
  if (maxsec) hstname << "s" << sec+minsec << "_";
  if (maxlay && !layer2lr) hstname << "p" << lay+minlay << "_";
  hstname.fill('0');
  hstname.width(2);
  hstname << str;
  if (leftright || layer2lr) hstname << (lr ? "R" : "L");
  return hstname.str();
}

int BankDescription::maxDescr = 0;
vector<BankDescription> gBankDescr;

void read_tdc_calib_banks() {
  string path ("/group/clas/parms");
  if(SC_Version_Flag == 2)
    path += "/tdc_calib_banks_v2.dat";
  else{path += "/tdc_calib_banks.dat";}

cout << "path = " << path << endl;  

  
  ifstream f(path.c_str());

  if (!f.good()) {
    cerr << "file " << path << " (bank specif.) not found in $CLAS_PARMS" << endl;
    throw "bank specifications not found";
  }
  while (f.good()) {
    char cc = f.peek();
    if (isalpha(cc)) {
      BankDescription bd;
      f >> bd;
      gBankDescr.push_back(bd);
    } 
    else {
      char dummy[256];
      f.getline(dummy,255);
    }   
  }

  cout << gBankDescr.size() << " bank descriptions read: ";

  for (vector<BankDescription>::iterator i=gBankDescr.begin(); 
       i!=gBankDescr.end(); i++) {
    cout << "[" << i->GetCode() << "," << i->GetBankName() << "] ";
  }
  cout << endl;
}

