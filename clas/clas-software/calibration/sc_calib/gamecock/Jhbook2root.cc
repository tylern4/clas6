#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <TF1.h>
#include <TF2.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TROOT.h>
#include <TRandom.h>
#include <TCanvas.h>
#include <TSystem.h>
#include <TRint.h>
#include "Jhbook2root.h"

using namespace std;

TFile*   gRootFile = NULL;
TRint*   gRootIntp = NULL;
TCanvas* gRootCnvs = NULL;

map<int,TH1*> gRootHist;

void throwId(string pfunction, int hid, string complain) {
  ostringstream message;
  message << pfunction << ": hist-id " << hid << ": " << complain;
  cerr << message.str() << endl;
  throw message.str().c_str();
}

int existsInMap(int hid) {
  map<int,TH1*>::iterator iter = gRootHist.find(hid);
  if (iter == gRootHist.end()) return 0;
  if (iter->second->InheritsFrom(TH2::Class())) return 2;
  if (iter->second->InheritsFrom(TH1::Class())) return 1;
  throwId("existsInMap", hid, "Object is not a histogram");
  return -1;
}

void rootcanvas(int width, int height) {
  gRootIntp = new TRint("interactive", 0, 0, 0, 0, 0);
  gRootCnvs      = new TCanvas("C", "C", 650, 900);
}

void rootrun() {
  if (!gRootIntp) 
    throw "rootrun: ROOT interpreter not found (you might want to call rootcanvas first)";
  gRootIntp->Run();
}

void rootupdate() {
  if (!gRootCnvs) 
    throw "rootupdate: ROOT canvas not found (you might want to call rootcanvas first)";
  gSystem->ProcessEvents();
  gRootCnvs->Modified();
  gRootCnvs->Update();
}

void rootopenw(const char* filename) {
  gRootFile = new TFile(filename, "recreate");
}

void rootend() {
  if (!gRootFile) throw "rootend: attempt to write in non existing root file";
  gRootFile->Write();
  gRootFile->Flush();
}

void rootbook1(int hid, const char* title, 
	       int xbins, double xmin, double xmax) {
  if (existsInMap(hid)) throwId("rootbook1", hid, "histogram exists");
  ostringstream name;
  name << "h" << hid;
  gRootHist[hid] = new TH1F (name.str().c_str(), title, xbins, xmin, xmax);
}

void rootbook2(int hid, const char* title, 
	       int xbins, double xmin, double xmax,
	       int ybins, double ymin, double ymax) {
  if (existsInMap(hid)) throwId("rootbook2", hid, "histogram exists");
  ostringstream name;
  name << "h" << hid;
  gRootHist[hid] = new TH2F (name.str().c_str(), title, xbins, xmin, xmax,
		      ybins, ymin, ymax);
}

void rootf1(int hid, double x, double value) {
  switch (existsInMap(hid)) {
  case 0: 
    throwId("rootf1", hid, "histogram not found");
    break;
  case 1:
    ((TH1F*)gRootHist[hid])->Fill (x, value);
    break;
  case 2: 
    throwId("rootf1", hid, "attempt to fill 2D histogram");
    break;
  }  
}

void rootf2(int hid, double x, double y, double value) {
  switch (existsInMap(hid)) {
  case 0: 
    throwId("rootf2", hid, "histogram not found");
    break;
  case 1:
    throwId("rootf2", hid, "attempt to fill 1D histogram");
    break;
  case 2: 
    ((TH2F*)gRootHist[hid])->Fill (x, y, value);
    break;
  }  
}

void rootopera(int h1, const char* coperator, int h2, int h3, 
	       double c1, double c2) {
  if (!existsInMap(h2)) 
    throwId("rootopera", h2, "histogram not found");
  if (!existsInMap(h3)) 
    throwId("rootopera", h3, "histogram not found");
  TH1* hres = (TH1*) gRootHist[h2]->Clone();
  switch (coperator[0]) {
  case '/': 
    hres->Divide(gRootHist[h3]);   
    hres->Scale(c1/c2);
    break;
  case '*': 
    hres->Multiply(gRootHist[h3]); 
    hres->Scale(c1*c2);
    break;
  case '+': 
    hres->Scale(c1);
    hres->Add(gRootHist[h3],c2); 
    break;
  case '-': 
    hres->Scale(c1);
    hres->Add(gRootHist[h3],c2); 
    break;
  }
  gRootHist[h1] = hres;
}

void rootdraw(int hid) {
  switch (existsInMap(hid)) {
  case 1:
    ((TH1F*)gRootHist[hid])->Draw();
    break;
  case 2:
    ((TH2F*)gRootHist[hid])->Draw("colz");
    break;
  default:
    throwId("rootdraw", hid, "histogram not found");
    break;
  }
}

