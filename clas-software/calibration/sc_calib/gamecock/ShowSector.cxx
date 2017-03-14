#include "jglobal.h"
#include "Inx.h"
#include "ShowSector.h"

void compose_fname (char* s, const char* stub, const int index,
		    int style = 0);

void showsingle (Inx index);

const int xcanvas = 1024;
const int ycanvas = 768;
const double xborder = 0.001;
const double yborder = 0.001;
const double xpad = xcanvas / 10.;
const double ypad = ycanvas / 6.;


ShowSector::ShowSector(int sector_) : 
  JMultiple(N_SECTOR), sector(sector_), cursorpos(-1) {
  char canvasname[80];
  sprintf (canvasname, "%s: Sector %d", gCalib->GetWindowName(), sector);

  Csec = (TCanvas*) gROOT->FindObject("Csec");
  if (Csec) {
    Csec->Clear();
    Csec->SetTitle(canvasname);
  }
  else {
    Csec = new TCanvas("Csec", canvasname, xcanvas, ycanvas);
  }

  Csec->Divide(10, 6, xborder, yborder);
  for (int i=0; i<N_SECTOR; i++) {
    int cii = (sector-1)*N_SECTOR + i;
    Csec->cd(i+1);

    /// draw a graph
    if (gCalib->IsGraph()) {
      if (gGraph[0][cii] && gFitFn[0][cii]) {
	gGraph[0][cii]->Draw("AP");
	gFitFn[0][cii]->Draw("same");
      }
      else if(i < 48)
        cout << "Graph " << cii << " doesn't exist." << endl;
      else{cout << "Graph " << cii << " doesn't exist.  If source run prior to 55357 then don't worry about it." << endl;}
    }

    /// draw a histogram
    else {
       if (gHisto[0][cii]) {
	if (gCalib->IsTwoDim())  gHisto[0][cii] ->Draw("colz");
	else                    gHisto[0][cii] ->Draw("");
	if (gFitFn[0][cii]) {
	  gFitFn[0][cii]->Draw("same");
	}
      }
    }

    if (gCalib->IsLogy()) gPad->SetLogy();

    CreateBox (cii);

    /// draw sector number in pavelabel
    char pavetext[20];
    sprintf (pavetext, "%d", i+1);
    TPaveLabel* pl = new TPaveLabel(0.80,0.83,0.9999,0.9999,pavetext,"NDC");
    pl->Draw();

    /// to be updated
    gPad->Modified();
  }
  //  Csec->Update();
  /*
  for (int i=0; i<N_SECTOR; i++) {
    sprintf (padname, "%s_%d", "Csec", i+1);
    TPad* getPad = (TPad*) Csec->GetPrimitive(padname);
    if (!getPad) {
      cerr << "cannot find pad " << padname << endl; exit(1); }
    int cii = (sector-1)*N_SECTOR + i;
    CreateBox (getPad, cii);
  }
  */
  Csec->Update();
}

void ShowSector::Update(Inx index) {
  if (index.GetFSector() != sector) return;
  Csec->cd(index.GetFStripe());
  gPad->Modified();
  gPad->Update();
}

void ShowSector::SetCursor(Inx index) {
  if (index.GetFSector() != sector) return;  /// possibly choosen from (all) in JSurvey
  if (index == cursorpos) return;   /// nothing to do

  if (cursorpos >= 0) {
    int oldsec = cursorpos/N_SECTOR + 1;
    int oldpad = cursorpos%N_SECTOR + 1;
    if (oldsec == sector) {
      Csec->cd(oldpad);
      SetNormal(cursorpos);
      gPad->Modified();
    }
  }
  cursorpos = index.GetIndex();
  Csec->cd(index.GetFStripe());
  SetHighlight(cursorpos);
  gPad->Modified();
  Csec->Update();
}

void ShowSector::PointerLeftClick(Inx index) {
  //  cout << "Pointer click on " << n << endl;
  if ( gCalib->IsGraph() && !gGraph[0][index.GetIndex()] ||
      !gCalib->IsGraph() && !gHisto[0][index.GetIndex()]) { 
    cout << "selected non existing histogram/graph ["<< index << "]" << endl; 
    //    throw "ShowSector::PointerLeftClick: histogram don't exist"; 
  }
  showsingle (index);
}

