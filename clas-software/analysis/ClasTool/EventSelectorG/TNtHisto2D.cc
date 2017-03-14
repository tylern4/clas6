// Autogenerated Class (Source File)
// Author : G.Gavalian
// Date   : Wed Jun 27 18:02:49 EDT 2007
//

#include "TNtHisto2D.h"

ClassImp(TNtHisto2D)


TNtHisto2D::TNtHisto2D (const char *hname): TNtHisto(hname){
    fH_ = new TH2D(GetHistName().Data(),"",1,0.,1.,1,0.,1.);
    fH_->GetXaxis()->SetNdivisions(505);
    fEpsFileName = "default.eps";
    fVariable    = "x";
    fCutsString  = "";
}

TNtHisto2D::~TNtHisto2D (){
  // delete fH_;
}

//-----------------

//-----------------

void       TNtHisto2D::SetBins(int nx, double xmin, double xmax, int ny, double ymin , double ymax){
  fH_->SetBins(nx,xmin,xmax,ny,ymin,ymax);
}

void       TNtHisto2D::Fill(TTree *tree, const char *opt)
{
  TString  farg;
  farg  = fVariable;
  farg += ">>";
  farg += GetHistName();
  gDirectory->ls();
  printf("=>>> FILLING : %s  %s\n",farg.Data(),fH_->GetName());
  int nres = tree->Draw(farg.Data(),fCutsString.Data(),opt);
  printf("filled events: %d\n",nres);
}

//-----------------
TString    TNtHisto2D::GetHistName()
{
  TString  hist_name = "_hist_2d_";
  hist_name += GetName();
  return hist_name;
}
