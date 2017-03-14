#include "JEventCount.h"

bool JBuffer::IsNextBank () { 
  if (eof() ) return false;
  if (inx <= 0) throw "inx <= 0"; 
  return inx < used; 
}

void JBuffer::ReadLastblock() {
  int nread;
  inx = 0;
  fseek (ff, -12*len, SEEK_END);
  if ((nread = fread (b, 4, len, ff)) != len) {
    cerr << "nread=" << nread << " != len=" << len << endl;
    throw "nread != len";
  }
  used = (*this) [1];
  if (used > len) {
    cerr << "len=" << len << " < used=" << used << endl;
    throw "used > len";
  }
  inx = 13;
}

int JBuffer::ReadBlock () {
  if (feof(ff)) return -1;
  int nread;
  inx = 0;

  int* blockheader = (b) ? b : bhead;
  if ( (nread = fread (blockheader, 4, 13, ff) ) != 13) {
    if (!nread) return -2;  /// end of file
    cerr << "nread=" << nread << " != 13" << endl;
    throw "nread != len";
  }

  switch (blockheader[2]) {
  case 0x04030201: swap = false; break;
  case 0x01020304: swap = true;  break;
  default: cerr << "unknown block signature " 
		<< hex << blockheader[2] << dec << endl;
  }

  int len0 = (*this) [0];
  used     = (*this) [1];

  if (!b) {
    len = len0;
    //    cout << "JBuffer length: " << len << endl;
    b = new int [len+1000];
  }
  else {
    if (len != len0) { 
      cerr << "New block has different length " 
	   << len0 << "!=" << len << endl; 
      throw "different blocklen";  
    }
  }

  if (used > len) {
    cerr << "len=" << len << " < used=" << used << endl;
    throw "used > len";
  }
  
  if ((nread = fread (b + 13, 4, len-13, ff) ) != len-13) {
    cerr << "nread=" << nread << " != len-13=" << len-13 << endl;
    throw "nread != len";
  }

  inx = 13;
  return 0;
}

int JBuffer::operator[] (int index) {
  if (b && inx + index > len) {
    cerr << "inx=" << inx << " + index=" << index
	 << " >= len=" << len << endl;
    throw "index overrun";
  }
  if (swap) return GetSwap(index);
  int ii = inx + index;
  return b? b[ii] : bhead[ii];
}

int JBuffer::GetSwap (int index) {
  int ii = inx + index;
  int number = b? b[ii] : bhead[ii];
  int rebmun = 0;
  for (int i=0; i<4; i++) {
    unsigned int byte = ( number >> (i*8) ) & 0xFF ;
    rebmun |= (byte << ((3-i)*8));
  }
  return rebmun;
}

void JBuffer::ReadEventHeader () {
  unsigned int xcode   = (*this)[1];
  //  unsigned int blockNo = (*this)[2];
  //  unsigned int evlen   = (*this)[10];

  //  int nextev = inx + evlen + 11;
  string name = GetHolerith(3, 2);

  /*
  for (int k=1; k<11; k++) 
    if (k!=3 && k !=4) {
      cout.width(8);
      cout << " " << (*this)[k] << dec;
    }
  cout << endl;
  */

  // 65 cooked   24, 16  raw
  if (xcode == 65 || xcode == 24 || xcode == 16) inx+= 11;  /// gack hack
  else throw "unexpected ycode";

}

void JBuffer::ReadBank () {
  vector<JBosFileBank*> followup;

  if (bank) delete bank;
  JBosFileBank* tmp = bank = new JBosFileBank(this);
  inx += bank->headLength + bank->bankLength;

  /// bank continues in next block ?
  while (tmp->flagCont && tmp->flagCont != 3) {
    if (ReadBlock() ) {
      cerr << "Unexpected end of file, bank " << bank->name << " might be incomplete" << endl;
      break;
    }
    tmp = new JBosFileBank(this);
    inx += tmp->headLength + tmp->bankLength;
    followup.push_back(tmp);
  }

  if (followup.size()) {
    int sum = bank->bankLength;
    for (vector<JBosFileBank*>::iterator i = followup.begin();
	 i != followup.end(); i++) {

      if (bank->name != (*i)->name) {
	cerr << "bank <" << bank->name << "> <" << (*i)->name << ">" << endl;
	throw "follow up bank has different name";
      }
      if (sum != (*i)->offsetCont) throw "unexpected offset for follow up";
      sum += (*i)->bankLength;
    }

    int* tmpdata = new int[sum];
    memcpy (tmpdata, bank->data, bank->bankLength*sizeof(int));
    for (vector<JBosFileBank*>::iterator i = followup.begin();
	 i != followup.end(); i++) {
      memcpy (tmpdata + (*i)->offsetCont,
	      (*i)->data, (*i)->bankLength*sizeof(int));
    }
    delete[] bank->data;
    bank->data = tmpdata;
    bank->bankLength = sum;
    for (vector<JBosFileBank*>::iterator i = followup.begin();
	 i != followup.end(); i++) {
      delete (*i);
    }
  }
}

string JBuffer::GetHolerith (int index, int length) {
  int ii = inx + index;
  string s;
  length = (length > 20) ? 20: length;
  for (int j=0; j<length; j++) {
    for (int i=0; i<4; i++) {
      unsigned char c = (unsigned char) ( b[ii+j] >> (i*8) ) & 0xFF ;
      s += c;
    }
  }
  return s;
}

JBosFileBank::JBosFileBank (JBuffer* b): data(NULL) {
  headLength = (*b) [0];
  if (headLength < 10) throw "headLen < 10";
  sector     = (*b) [3];
  sizeofElem = (*b) [4];
  numberElem = (*b) [5];
  flagCont   = (*b) [6];
  offsetCont = (*b) [7];
  bankLength = (*b) [8];
  if (bankLength < 1) { 
    cout << "\n----------------\n";
    cout << b->inx << " " << b->GetNused() << " || ";
    for (int i=0; i<15; i++) {
      cout << hex << (*b) [i] << " ";
    }
    cout << endl;
    throw "bankLen < 1";
  }
  name   = b->GetHolerith(1,1);
  format = b->GetHolerith(9,headLength-9);
  data = new int [bankLength];
  int max = bankLength;

  if (b->inx + max > b->GetNused() || b->inx + max > b->GetLength() ) {
    cerr << "error reading bank " << name 
	 << ", inx=" << b->inx << ", bankLength=" << bankLength 
	 << ", used=" << b->GetNused()
	 << ", bufsize=" << b->GetLength() << endl;
    max = b->GetNused() - b->inx;
  }
  for (int i=0; i< max; i++) {
    data [i] = (*b) [headLength+i];
  }
}
  
string JBosFileBank::GetKey() {
  ostringstream oss;
  oss << name;
  if (sector) oss << " " << sector;
  return oss.str();
}

JBosFileBank JBosFileBank::operator= (const JBosFileBank b) {
  headLength = b.headLength;
  bankLength = b.bankLength;
  name       = b.name;
  format     = b.format;
  sector     = b.sector;
  sizeofElem = b.sizeofElem;
  numberElem = b.numberElem;
  flagCont   = b.flagCont;
  offsetCont = b.offsetCont;
  data = new int [bankLength];
  memcpy (data, b.data, bankLength*sizeof(int));

  return (*this);
}

bool operator== (const JBosFileBank a, const JBosFileBank b) {
  if (a.headLength != b.headLength) return false;
  if (a.bankLength != b.bankLength) return false;
  if (a.name != b.name)             return false;
  if (a.format != b.format)         return false;
  if (a.sector != b.sector)         return false;
  if (a.sizeofElem != b.sizeofElem) return false;
  if (a.numberElem != b.numberElem) return false;
  if (a.flagCont != b.flagCont)     return false;
  if (a.offsetCont != b.offsetCont) return false;
  if (memcmp (a.data, b.data, a.bankLength*sizeof(int))) return false;
  return true;
}

ostream& operator<< (ostream& os, const JAtom a) {
  switch (a.format) {
  case fm_unknown: os << " <?>" ; break;
  case fm_int:     os << " <" << a.z << ">" ; break;
  case fm_float: 
    {
      float* ptr = (float*) &a.z;
      os << " <" << *ptr << ">" ;
    }
    break;
  case fm_short:   os << " <" << (a.z&0xFFFF) << "," << (a.z>>16) << ">" ; 
    break;
  case fm_byte: 
    {
      os << " <"; 
      int zz = a.z;
      for (int i=0; i<4; i++) {
	if (i) os << ",";
	os << (zz & 0xFF);
	zz >>= 8;
      }
      os << ">";
    }
    break;
  }
  return os;
}

ostream& operator<< (ostream& os, const JBosFileBank b) {
  vector<formatid_t> vf;
  string stmp = b.format;
  os << "<" << stmp << ">" << endl;

  /// convert  )(,  to  blank
  unsigned int ipos;
  while ( (ipos = stmp.find_first_of(",()")) != string::npos) stmp[ipos] = ' ';
  
  /// parse string
  istringstream sform(stmp);
  while (sform.good()) { 
    string item;
    sform >> item;
    if (sform.fail()) break;
    
    formatid_t fid = fm_unknown;
    int repeat = 1;
    
    
    if (item == string("B16")) {
      repeat = b.bankLength;
      fid    = fm_short;
    }
    else if (item == string("B08")) {
      repeat = b.bankLength;
      fid    = fm_byte;
    }
    
    else {
      
      /// last char reveals type
      switch ( item[item.length()-1] ) {
      case 'I': fid = fm_int; break;
      case 'F': fid = fm_float; break;
      default: cerr << "format not understood: <" << item << ">" << endl;
	throw "bad format";
      }  
      
      /// get repetitions
      if (item.length() > 1) {             
	istringstream srepeat(item.substr(0,item.length()-1));
	srepeat >> repeat;
      }
    }
    for (int i=0; i<repeat; i++) {
      vf.push_back (fid);
    }
  }    


  int nloop = 0;
  if (vf.size() == b.bankLength) {
    nloop = 1;
  }

  else if (vf.size() * b.numberElem == b.bankLength) {
    nloop = b.numberElem;
  }
  
  else {
    cerr << vf.size() << " x " << b.numberElem << "!=" << b.bankLength << endl;
    throw ("format missmatch");
  }

  int k=0;
  for (int j=0; j<nloop; j++) {
    for (unsigned int i=0; i<vf.size(); i++) {
      JAtom a(b.data[k++], vf[i]);
      os << a;
    }
    os << endl;
  }

  return os;
}

typedef map <string, JBosFileBank> bankmap_t;
typedef map <string, JBosFileBank>::iterator bankiter_t;

bool getEvent(JBuffer * buf, bankmap_t * bankmap) {
  if (buf->eof()) return false;
  if (!buf->IsNextBank()) {
    if (buf->ReadBlock()) return false;
  } 

  while ((*buf)[0] != 0x04030201) {
    string key;
    buf->ReadBank();
    JBosFileBank*bk = buf->GetBank();
    (*bankmap)[bk->GetKey()] = *bk;
    //    cout.width(8);
    //    cout << bk->GetKey() << " "  << bk->bankLength << endl;

    if (bk->name == "HEAD") {
      for (unsigned int i=0; i< bk->bankLength; i++) {
	cout << bk->data[i] << " " ;
      }
      cout << endl;
    }
    if (!buf->IsNextBank()) {
      if (buf->ReadBlock()) return false;  /// gack hack: check for banks read 
    }
  }

  if (buf->eof()) return false;

  if ((*buf)[0] == 0x04030201) {
    buf->ReadEventHeader();
    //    cout << "------------------" << endl;
  }
  return true;
}

JEventCount::JEventCount(const char* filename): ok(false), first(0), last(0) {
  try {
    FILE * fa;

    if ( (fa = fopen(filename, "r")) == NULL) { 
      cerr << "can't open " << filename << endl; throw "error open file";}
    
    JBuffer bufa(fa);

    while (!first && !bufa.ReadBlock()) {
      while (  bufa.IsNextBank() ) {
	
	int code = bufa[0];
	if (code == 0x04030201) {
	  bufa.ReadEventHeader();
	}
	else {
	  bufa.ReadBank();
	  JBosFileBank*bk = bufa.GetBank();
	  if (bk->name=="HEAD") {
	    first = bk->data[2];
	    if (first > 0) break;
	  }
	}
      }
    }
    
    bufa.ReadLastblock ();
    
    while (!bufa.eof()) {
      while (  bufa.IsNextBank() ) {
	
	int code = bufa[0];
	if (code == 0x04030201) {
	  bufa.ReadEventHeader();
	}
	else {
	  bufa.ReadBank();
	  JBosFileBank*bk = bufa.GetBank();
	  if (bk->name=="HEAD") {
	    last = bk->data[2];
	  }
	}
      }
      if (bufa.ReadBlock()) break;
    }

    ok = true;
  }
  catch (const char* e) {
    cerr << "Exeception <" << e << ">" << endl;
  }
}

JFileInfo::JFileInfo(string name_): 
  JEventCount(name_.c_str()), name(name_), bsize(0) 
{
  struct stat mystat;
  if (stat (name.c_str(), &mystat)==0) {
    bsize  = mystat.st_blocks;
  }
}
  
double JFileInfo::GetPercent(int eventNo) {
  if (!good()) return perc0;
  if (eventNo < GetFirst()) return perc0;
  if (eventNo > GetLast()) return perc0;

  return perc0 + (perc1 - perc0) * (eventNo - GetFirst()) / GetDiff();
}

JFileCount::JFileCount(vector<string>* v_) {
  for (vector<string>::iterator i =(*v_).begin(); i != (*v_).end(); i++) {
    v.push_back (JFileInfo(*i));
  }
  Constructor();
}

JFileCount::JFileCount(int nfiles, char* filelist[]) {
  for (int i = 0; i<nfiles; i++) {
    v.push_back (JFileInfo(filelist[i]));
  }
  Constructor();
}

void JFileCount::Constructor() {
  double sum = 0;
  for (vector<JFileInfo>::iterator i = v.begin(); i != v.end(); i++) {
    double nextsum = sum + i->GetBsize();
    i->SetPercent(sum, nextsum);
    sum = nextsum;
  }
  if (sum <= 0)  return;
  for (vector<JFileInfo>::iterator i = v.begin(); i != v.end(); i++) {
    i->SetScale(100./sum);
  }
}

void count_initialize(int nfiles, char* filelist[]) {
  gFileCount = new JFileCount(nfiles, filelist);
}

double get_percent(int ifile, int eventNo) {
  if (!gFileCount) return 0.;
  return gFileCount->GetPercent(ifile, eventNo);
}
