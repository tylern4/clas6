{
 TFile f("eff.root");
 int Sec = 7;
 TH2F HHl[Sec];
 TH2F HHu[Sec];

 gStyle->SetPadRightMargin(0.05);
 gStyle->SetPadLeftMargin(0.1);

 TCanvas *EFF1 = new TCanvas("EFF1","EFF1", 0, 0, 800, 1000);
 TCanvas *EFF2 = new TCanvas("EFF2","EFF2", 0, 0, 800, 1000);
 EFF1->Range(0,0,21.5,28);
 EFF2->Range(0,0,21.5,28);
 EFF1->Divide(1,2);
 EFF2->Divide(1,2);

 for(int s=1; s<Sec; s++)
 {
  HHl[s] = (TH2F)f.Get(Form("WireMap_Sector%d",s));
  HHu[s] = (TH2F)f.Get(Form("WireMap_Sector%d",s));

  HHl[s].GetXaxis()->SetRange(0,96);
  HHl[s].GetXaxis()->SetTitle(Form("Wire number: Lower half       Sector %d     ", s));
  HHl[s].GetYaxis()->SetTitle("Layer number         ");

  HHu[s].GetXaxis()->SetRange(97,192);
  HHu[s].GetXaxis()->SetTitle(Form("Wire number: Upper half       Sector %d     ", s));
  HHu[s].GetYaxis()->SetTitle("Layer number         ");

  HHl[s].GetZaxis()->SetNdivisions(503);
  HHu[s].GetZaxis()->SetNdivisions(503);

  EFF1->cd(1);
  HHl[s].GetXaxis()->SetTitleOffset(1.2);
  HHl[s].GetYaxis()->SetTitleOffset(1.1);
  HHl[s].Draw("box");

  EFF1->cd(2);
  HHu[s].GetXaxis()->SetTitleOffset(1.2);
  HHu[s].GetYaxis()->SetTitleOffset(1.1);
  HHu[s].Draw("box");
  //EFF1->Print(Form("WireMap_box_Sector%d.eps",  s));
  EFF1->Print(Form("WireMap_box_Sector%d.gif",  s));


  EFF2->cd(1);
  gPad->SetTheta(70);
  gPad->SetPhi(305);
  HHl[s].GetXaxis()->SetTitleOffset(2.2);
  HHl[s].GetYaxis()->SetTitleOffset(1.8);
  HHl[s].Draw("lego2");

  EFF2->cd(2);
  gPad->SetTheta(70);
  gPad->SetPhi(305);
  HHu[s].GetXaxis()->SetTitleOffset(2.2);
  HHu[s].GetYaxis()->SetTitleOffset(1.8);
  HHu[s].Draw("lego2");


//  EFF2->Print(Form("WireMap_lego_Sector%d.eps", s));
  EFF2->Print(Form("WireMap_lego_Sector%d.gif", s));
 }

}
