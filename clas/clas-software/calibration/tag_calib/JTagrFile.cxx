extern "C" {
#include "ntypes.h"
#include "bostypes.h"
#include "call.h"
}

#include "JBosBank.h"
#include "JTagrFile.h"

using namespace std;

JTagrFile::JTagrFile (string fileName, ios_base::openmode flags_) : 
  f(NULL), flags(flags_) {

  if (flags != ios::out) flags = ios::in;

  f = new fstream(fileName.c_str(), flags);
}

JTagrFile::~JTagrFile() {
  if (f) delete f;
}

void JTagrFile::write() {
  tagr_t  header;
  char* data;
  if (!f || flags != ios::out) throw "invalid write attempt";
  if (!f->good()) return;
  Jhead head;
  Jtagr tagr;
  if (! head.Found()) throw "header bank not found";
  header.t_id = 0xABCD;
  header.e_id = head[0].nevent;
  header.stat = (tagr.Found()? tagr.GetNrows() : 0);
  header.erg  = head[0].nrun;
  Jrft  rft;
  if (rft.Found()) {
    dropAllBanks(&bcs_, "CL01");
    make_cl01_bank_using(3);
    Jcl01 cl01a;
    header.ttag = cl01a[0].rf;
    dropAllBanks(&bcs_, "CL01");
    make_cl01_bank_using(4);
    Jcl01 cl01b;
    header.tpho = cl01b[0].rf;
    dropAllBanks(&bcs_, "CL01");
    make_CL01_bank();
  }
  else {
    header.ttag = head[0].time;
    header.tpho = head[0].trigbits;
  }
  f->write((char*) &header, sizeof(tagr_t));
  if (!tagr.Found()) return;
  data = (char*) &tagr[0];
  f->write(data, sizeof(tagr_t) * tagr.GetNrows());
}

void JTagrFile::read() {
  if (!f || flags != ios::in) throw "invalid read attempt";
  if (!f->good()) return;

  int loop = 0;
  int nrows = 0;

  Jhead head;
  if (!head.Found()) throw "HEAD bank not found";
  dropAllBanks(&bcs_,"TAGITAGR");

  tagr_t  header;
  while (f->good()) {
    f->read((char*) &header, sizeof(tagr_t));
    if (header.t_id == 0xABCD) {
      loop++;
      nrows = header.stat;
      if (header.e_id == head[0].nevent) break;
      if (!(loop%100)) {
	cout << "Fast forward "; cout.width(6); cout << loop << "\r"; cout.flush();
      }
    }
  }
  if (loop > 100) {
    cout << "Fast forward "; cout.width(6); cout << loop << endl;;
  }

  if (!nrows) return;

  clasTAGR_t* TAGR= (clasTAGR_t*) 
    makeBank(&bcs_, "TAGR", 0, sizeof(tagr_t)/sizeof(int), nrows);
  if (!TAGR) throw "can't create TAGR bank";
  f->read((char*) TAGR->tagr, sizeof(tagr_t)*nrows);
}

bool JTagrFile::good() {
  if (!f) return false;
  return f->good();
}

