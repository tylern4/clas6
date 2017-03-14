#include "jdefine.h"

double f_tdc(Double_t *x, Double_t *par) {
  return par[0]+par[1]*(*x)+par[2]*(*x)*(*x);
}

void dummy() {
  TGraphErrors* gGraph[N_SECTOR]; 
  TF1*          gFitFn[N_SECTOR];

  TFile* ff = new TFile ("testg.root");
  if (!ff) return;


  for (int i=0; i<N_SECTOR; i++) {
    char graph_name [80];
    sprintf (graph_name, "L%d.%02d", i/N_SECTOR+1, i%N_SECTOR+1); 
    gGraph[i]=(TGraphErrors*) ff->Get(graph_name);
    if (gGraph[i] == NULL) {
      cerr << "graph " << graph_name << "not found in file" << endl;
      gGraph[i]=NULL;
      gFitFn[i]=NULL;
      continue;
    }
    
    char fname [80];
    sprintf (fname, "ftdc%d", i);
    //    TF1 *ftdc = new TF1(fname ,f_tdc,0.,4096.,3);
    TF1* ftdc = new TF1(fname, "[0]+[1]*x+[2]*x*x", 0., 4096.);
    ftdc->SetParameters(0.1,50.,0.00001);
    ftdc->SetLineColor(4);

    int fitError = gGraph[i]->Fit(ftdc,"0q");
    if (fitError) {
      gFitFn[i] = NULL;
      cerr << "fit for " << graph_name << " returns " << fitError << endl;
      continue;
    }
    
    gFitFn[i] = ftdc;

    cout << i << "\t" << fitError  << "\t" << ftdc->GetParameter(1)  
	 << "\t" << ftdc->GetParameter(2) << endl;
  }
  
  TCanvas* Csec = new TCanvas("Csec", "Csec", 1024, 768);
  Csec->Divide(10, 6, 0.001, 0.001);

  for (int i=0; i<N_SECTOR; i++) {
    Csec->cd(i+1);
    if (gGraph[i]) {
      gGraph[i]->Draw("AP");
      if (gFitFn[i]) {
	gFitFn[i]->Draw("same");
      }
    }
  }  
}


