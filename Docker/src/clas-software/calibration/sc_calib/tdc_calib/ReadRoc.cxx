#include "ReadRoc.h"

extern vector<BankDescription> gBankDescr;
extern int SC_Version_Flag;

ReadRoc::ReadRoc (BankDescription* bd_) : bd(bd_) 
{
  char line [1024];
  char filename[256];
  int lineCount = 0;
  int fileCount = 0;

  string path ("/group/clas/parms");
  if(SC_Version_Flag == 2)
    path += "/TT_v2";
  else{path += "/TT";}

  rc = new RocChannel[bd->GetMaxIndex()];
  int nentry[bd->GetMaxIndex()];
  memset (nentry, 0, sizeof(nentry));

  for (int i = 10; i < 33; i++) {
    int iline = 0;
    sprintf (filename, "%s/ROC%02d.tab", path.c_str(), i);
//cout << "filename = " << filename << endl;
    ifstream f(filename, ios::in);
    if (f.good()) fileCount++;

    while (f.good ()) {
      f.getline (line, 1023);
      iline++;
      if (! strlen(line)) continue;
      if (*line == '#') continue;
      try {
	int index = parseLine(line);
	if (index >= 0) {
	  lineCount ++;
	  nentry[index] ++;
	}
      }
      catch (const char* e) {
	cout << "<" << line << ">" << endl;
	cout << filename << ":" << iline << " <" << e << ">" << endl;
	throw e;
      }
    }
  }

  if (fileCount != 23) {
    cout << "number of ROC files found: " << fileCount << endl;
  }

  if (lineCount != bd->GetMaxIndex()) {
    cout << "lineCount!=" << lineCount << "\tmaxinx=" << bd->GetMaxIndex() << endl;
    throw "ROC: didn't find all channels";
  }

  for (int i = 0; i<bd->GetMaxIndex(); i++) {
    if (nentry[i] != 1) {
      cout << "nentry[" << i << "] = " << nentry[i] << endl;
      throw "ROC: multiple or no ROC-entries found for channel";
    }
  }
}

RocChannel::RocChannel(char* cline) {
  istringstream line (cline);
  line.setf(ios::hex & ios::dec , ios::basefield);
  line >> bank;
  int column = 1;
  while (line.good() && column < 11) {
    unsigned int temp;
    line >> temp;
    switch (column++) {
    case 1: sector = temp; break;
    case 2: layer  = temp; break;
    case 3: stripe = temp; break;
    case 4: lrinx  = temp; break;
    case 5: break;  // tag
    case 6: break;  // stat
    case 7: crate  = temp; break;
    case 8: slot   = temp; break;
    case 9: type   = temp; break;
    case 10: channel = temp; break;
    default: throw "ROC: invalid colunn";
    }
  }
  if (column != 11) {
    cout << "column = " << column << endl;
    throw "ROC: line doesn't have 11 columns";
  }

  if((bank != "ECT") && (bank != "EC"))
    layer = layer & 0x1;
}

// returns -1 if line does not contain information for this detector
// returns detector index otherwise
int ReadRoc::parseLine (char* cline) {
  RocChannel roc(cline);

  // not the bank we are looking for ??
  if (roc.bank != bd->GetBankName())   return -1;  

  if (roc.type != bd->GetModuleType())   return -1;  // not the right module

  // stripe not in the range
  if (roc.stripe < bd->GetMinStripe() || bd->GetMaxStripe() < roc.stripe)
    return -1;

  if (roc.sector < bd->GetMinSector() 
      || bd->GetMaxSector() < roc.sector)
    throw "ROC: invalid sector";
  if (roc.layer  < bd->GetMinLayer() || bd->GetMaxLayer() < roc.layer) {
    cerr << "Layer \tmin=" << bd->GetMinLayer() << "\tmax=" 
	 << bd->GetMaxLayer() << "\tlay=" << roc.layer  << endl;
    throw "ROC: invalid layer";
  }

  switch (roc.lrinx) {    // accept zero, 1, 2 (bkwds compat?), 3
  case 0: break;
  case 1: if (bd->IsLeftRight()){roc.lrinx = 0; break;}
  case 2: if (bd->IsLeftRight()) break; 
  case 3: if (bd->IsLeftRight()){roc.lrinx = 2; break;}
  default:
    throw "ROC: invalid left/right code (should be 0 -> 3)";
  }

  if (bd->IsLayer2LR()) {
    switch (roc.layer) {
/*    case 0: roc.lrinx = 2; break;
    case 1: roc.lrinx = 0; break; */ //old and backwards

    case 0: roc.lrinx = 0; break;
    case 1: roc.lrinx = 2; break;
    default: 
      throw "ROC: invalid layer for SCT";
    }
    roc.layer = 0;
  }

  int multlay = bd->GetMaxLayer() ? bd->GetMaxLayer() : 1;
  int multsec = bd->GetMaxSector() ? bd->GetMaxSector() : 1;
  int index = roc.stripe - bd->GetMinStripe();
  if (bd->GetMinLayer()) {
    index += (roc.layer-bd->GetMinLayer()) * bd->GetMaxStripe();
  }

  if (bd->GetMinSector()) {
    index += (roc.sector-bd->GetMinSector()) 
      * multlay * bd->GetMaxStripe() ;
  }
  index += (roc.lrinx /2) 
    * multsec * multlay * bd->GetMaxStripe();

  roc.SetID(index);
  rc [index] = roc;

  return index;
}

void ReadRoc::show () {
  cout << "printing " << bd->GetMaxIndex() << " channels" << endl;
  for (int i = 0; i < bd->GetMaxIndex(); i++) {
    cout.width (4);
    cout << i << "  Crate Slot Chan [";
    cout.width (2);
    cout << rc [i].crate << ".";
    cout.width (2);
    cout << rc [i].slot << ".";
    cout.width (2);
    cout << rc [i].channel << "]" << endl;
  }
}

bool RocChannel::operator< (const RocChannel rc) const {
  if (crate < rc.crate) return true;
  if (crate > rc.crate) return false;
  if (slot  < rc.slot)  return true;
  if (slot  > rc.slot)  return false;
  return (channel < rc.channel);
}

bool RocChannel::operator> (const RocChannel rc) const {
  if (crate > rc.crate) return true;
  if (crate < rc.crate) return false;
  if (slot  > rc.slot)  return true;
  if (slot  < rc.slot)  return false;
  return (channel > rc.channel);
}

bool RocChannel::operator!= (const RocChannel rc) const {
  return !(*this == rc);
}

bool RocChannel::operator== (const RocChannel rc) const {
  if (crate != rc.crate) return false;
  if (slot  != rc.slot)  return false;
  return (channel == rc.channel);
}
