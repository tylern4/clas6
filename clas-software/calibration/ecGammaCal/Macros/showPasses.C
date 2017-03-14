void showPasses(const char* szFilenameTemplate, int nPasses)
{
  gStyle->SetOptFit(11);
  gStyle->SetOptStat(11);
  
  TFile* f = new TFile[nPasses + 1];
  TTree* tree = new TTree[nPasses + 1];

  char szFilename[200];
 
  for (int i = 0; i <= nPasses; i++) {
    sprintf(szFilename, "%s%d.root", szFilenameTemplate, i + 1);
    
    f[i] = TFile::Open(szFilename);
    tree[i] = (TTree*) f[i]->Get("tree");
  }

  TCanvas *c1 = new TCanvas("c1", "", 165, 160, 750, 880);
  int nRows = ceil(nPasses / 2.); 
  c1->Divide(2, nRows);
  
  TH1F *h1[50];
  char szHistRef[10];
  char szHistTitle[100];
  char szTreeDraw[100];
  for (int i = 0; i < nPasses; i++) {
    c1->cd(i + 1);
    sprintf(szHistRef, "h1_%d", i + 1);
    sprintf(szHistTitle, "T_{expected} - T_{model} integrated (ns), pass %d", i + 1);
    sprintf(szTreeDraw, "texpect - tmodel >> %s", szHistRef);
    h1[i] = new TH1F(szHistRef, szHistTitle, 100, -5. , 5.);
    tree[i]->Draw(szTreeDraw);
    c1->Update();
  }

  c1->Print("calib_passes.eps");

  // final result
  TCanvas *c2 = new TCanvas("c2", "", 1);
  c2->cd();
  
  sprintf(szHistRef, "h1_%d", nPasses + 1);
  sprintf(szHistTitle, "T_{expected} - T_{model} integrated (ns),  all #gamma with pass %d constants",  nPasses + 1);
  sprintf(szTreeDraw, "texpect - tmodel >> %s", szHistRef);
  h1[nPasses] = new TH1F(szHistRef, szHistTitle, 100, -5. , 5.);
  h1[nPasses]->GetXaxis()->SetTitle("T_{expected} - T_{model} (ns)");
  h1[nPasses]->GetYaxis()->SetTitle("Number of hits");
  h1[nPasses]->GetYaxis()->CenterTitle();
  h1[nPasses]->GetYaxis()->SetTitleOffset(1.3);
  sprintf(szTreeDraw, "texpect - tmodel >> %s", szHistRef);
  tree[nPasses]->Draw(szTreeDraw);
  TF1 f1x_1("f1x_1","gaus", -1., 1.);
  TF1 f1x_2("f1x_2","pol3", -3., 3.);
  TF1 f1x_T("f1x_T","gaus(0)+pol3(3)", -3., 3.);
 
  h1[nPasses]->Fit("f1x_1", "R0");
  h1[nPasses]->Fit("f1x_2", "R0");
 
  Double_t par[6];
  f1x_1->GetParameters(&par[0]);
  f1x_2->GetParameters(&par[3]);
  f1x_T->SetParameters(par);
  f1x_T->SetLineColor(2);
  f1x_T->SetParNames("A", "x_{0}", "#sigma", "a_{0}" ,"a_{1}", "a_{2}", "a_{3}");
  h1[nPasses]->Fit("f1x_T","R0");
  
  c2->Update();
  c2->Print("calib_final.eps");

}



