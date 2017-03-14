#include "jdefine.h"

void testgfit() {
  TGraphErrors* gGraph[N_SECTOR]; 
  TF1*          gFitFn[N_SECTOR];

  // open file with TGraphErrors
  TFile* ff = new TFile ("root/testg.root");
  if (!ff) { cerr << "can't open testg.root" << endl; return; }

  int badcount = 0;
  for (int i=0; i< N_SECTOR; i++) {

    // get the TGraphErrors (works fine, found all)
    char graph_name [80];
    sprintf (graph_name, "L1.%02d", i+1); 
    gGraph[i]=(TGraphErrors*) ff->Get(graph_name);
    if(gGraph[i] == NULL) {
      cerr << "graph " << graph_name << "not found in file" << endl;
      badcount++;
      gGraph[i]=NULL;
      gFitFn[i]=NULL;
      continue;
    }
    gGraph[i]->GetListOfFunctions()->Clear();
    
    // create the fit function (works fine, created all)
    char funct_name [80];
    sprintf (funct_name, "ftdc%d", i);
    TF1* ftdc = new TF1(funct_name, "[0]+[1]*x+[2]*x*x", 0., 4096.);
    ftdc->SetParameters(2.,0.05,0.00001);
    ftdc->SetLineColor(4);

    // do the fit (fitError always 0, indicating successful fit)
    int fitError = gGraph[i]->Fit(ftdc,"0q");
    if (fitError) {
      gFitFn[i] = NULL;
      cerr << "fit for " << graph_name << " returns " << fitError << endl;
      continue;
    }
    
    // store the function to use it later
    gFitFn[i] = ftdc;

    // all fit parameter very resonable
    cout << i <<  "\t" << gFitFn[i]->GetParameter(1)  
	 << "\t" << gFitFn[i]->GetParameter(2) << endl;
  }
  
  // create canvas to show the results
  TCanvas* Csec = new TCanvas("Csec", "Csec", 1024, 768);
  Csec->Divide(10, 6, 0.001, 0.001);

  // *** Error caused by TGraph::Paint() *** in this loop
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


