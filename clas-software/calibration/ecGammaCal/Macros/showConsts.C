int showConsts(const char* szFileName)
{
  ifstream fsConsts(szFileName);
  if (! fsConsts) {
    cerr << "Couldn't open " << szFileName << endl;
  }

  const int c_nTubes = 1296; // 1297 tubes
 
  // parameters constants;
  double adA0[c_nTubes], adA1[c_nTubes], adA2[c_nTubes], adA3[c_nTubes], adA4[c_nTubes];
  double adTubes[c_nTubes];

  const int c_nValues = 11; // number of values on a single line 
  double adValues[c_nValues];

  for (int nTube = 0 ; nTube < c_nTubes; nTube++) {
    // fills the x-axis
    adTubes[nTube] = nTube + 1;
    // fills the 
    for (int nValue = 0; nValue < c_nValues; nValue++) {
      fsConsts >> adValues[nValue];
    }
    //
    if (adValues[2] == -1000.) {
      adA0[nTube] = adA1[nTube] = adA2[nTube] =  adA3[nTube] = adA4[nTube] = 0.;
    }
    else {
      adA0[nTube] = adValues[1];
      adA1[nTube] = adValues[3];
      adA2[nTube] = adValues[5];
      adA3[nTube] = adValues[7];
      adA4[nTube] = adValues[9];
    }
  }


  TCanvas *c1 = new TCanvas("c1", "", 165, 160, 750, 880);     
  c1->Divide(2,3);
  
  // 1st parameter
  c1->cd(2);
  TH2F *h2_0 = new TH2F("h2_0", "parameter a_{0} versus tube", 1296, 0, 1296, 100, 0., 30.);
  h2_0->GetXaxis()->SetTitle("tube index");
  h2_0->GetYaxis()->SetTitle("a_{0}");
  h2_0->GetYaxis()->CenterTitle();
  h2_0->SetStats(kFALSE);
  h2_0->Draw();
  TGraph* grA0 = new TGraph(c_nTubes, adTubes, adA0);
  grA0->SetMarkerStyle(6);
  grA0->SetMarkerColor(1);
  grA0->Draw("P");

  // 2nd parameter
  c1->cd(3);
  TH2F *h2_1 = new TH2F("h2_1", "parameter a_{1} versus tube", 1296, 0, 1296, 100, 0., 0.1);
  h2_1->GetXaxis()->SetTitle("tube index");
  h2_1->GetYaxis()->SetTitle("a_{1}");
  h2_1->GetYaxis()->CenterTitle();
  h2_1->SetStats(kFALSE);
  h2_1->Draw();
  TGraph* grA1 = new TGraph(c_nTubes, adTubes, adA1);
  grA1->SetMarkerStyle(6);
  grA1->SetMarkerColor(1);
  grA1->Draw("P");

  // 3nd parameter
  c1->cd(4); 
  TH2F *h2_2 = new TH2F("h2_2", "parameter a_{2} versus tube", 1296, 0, 1296, 100, -50., 0.);
  h2_2->GetXaxis()->SetTitle("tube index");
  h2_2->GetYaxis()->SetTitle("a_{2}");
  h2_2->GetYaxis()->CenterTitle();
  h2_2->SetStats(kFALSE);
  h2_2->Draw();
  TGraph* grA2 = new TGraph(c_nTubes, adTubes, adA2);
  grA2->SetMarkerStyle(6);
  grA2->SetMarkerColor(1);
  grA2->Draw("P");
  
  // 4th parameter
  c1->cd(5);
  TH2F *h2_3 = new TH2F("h2_3", "parameter a_{3} versus tube", 1296, 0, 1296, 100, -0.0005, 0.);
  h2_3->GetXaxis()->SetTitle("tube index");
  h2_3->GetYaxis()->SetTitle("a_{3}");
  h2_3->GetYaxis()->CenterTitle();
  h2_3->SetStats(kFALSE);
  h2_3->Draw();
  TGraph* grA3 = new TGraph(c_nTubes, adTubes, adA3);
  grA3->SetMarkerStyle(6);
  grA3->SetMarkerColor(1);
  grA3->Draw("P");
  
  // 5th parameter
  c1->cd(6); 
  TH2F *h2_4 = new TH2F("h2_4", "parameter a_{4} versus tube", 1296, 0, 1296, 100, 0., 3e-7);
  h2_4->GetXaxis()->SetTitle("tube index");
  h2_4->GetYaxis()->SetTitle("a_{3}");
  h2_4->GetYaxis()->CenterTitle();
  h2_4->SetStats(kFALSE);
  h2_4->Draw();
  TGraph* grA4 = new TGraph(c_nTubes, adTubes, adA4);
  grA4->SetMarkerStyle(6);
  grA4->SetMarkerColor(1);
  grA4->Draw("P");

  c1->Print("calib_parameters.eps");

  fsConsts.close();

}
