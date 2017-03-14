Double_t sigmaBeta0(Double_t dBeta0, Double_t dSigmaT, Double_t dSigmaL)
{
  Double_t dL = 500.; // 5 meters from target to EC
  Double_t dC = 30; // spped of light: 30 cm/ns
  
  Double_t dSigmaBeta0 = dBeta0 / dL * (dSigmaL + dBeta0 * dC * dSigmaT);

  return (dSigmaBeta0);
}


Double_t betaNeutron(Double_t* x, Double_t *par)
{
  
  Double_t dMom = x[0];
  Double_t dMass = 0.939; // in GeV
  Double_t dBeta = dMom / sqrt(dMass * dMass + dMom * dMom);

  return (dBeta);
} 


Double_t betaNDist(Double_t* x, Double_t* par)
{
  Double_t dBeta = x[0];
  Double_t dSigmaT = par[0];
  Double_t dMom = par[1];

  Double_t dBeta0 = betaNeutron(&dMom, NULL);
  Double_t dSigmaL = 20; // 20 cm
  Double_t dSigmaBeta0 =  sigmaBeta0(dBeta0, dSigmaT, dSigmaL);
  Double_t dFactor1 = 1. / sqrt(2. * TMath::Pi()) / dSigmaBeta0;
  Double_t dFactor2 = exp(-0.5 * (dBeta - dBeta0) * (dBeta - dBeta0) / dSigmaBeta0 / dSigmaBeta0);

  return (dFactor1 * dFactor2);
  
}


Double_t betaGDist(Double_t* x, Double_t* par)
{
  Double_t dBeta = x[0];
  Double_t dSigmaT = par[0];

  Double_t dBeta0 = 1.;
  Double_t dSigmaL = 1.;
  Double_t dSigmaBeta0 =  sigmaBeta0(dBeta0, dSigmaT, dSigmaL);
  Double_t dFactor1 = 1. / sqrt(2. * TMath::Pi()) / dSigmaBeta0;
  Double_t dFactor2 = exp(-0.5 * (dBeta - dBeta0) * (dBeta - dBeta0) / dSigmaBeta0 / dSigmaBeta0);

  return (dFactor1 * dFactor2);
  
}


Double_t lostNeutrons(Double_t* x, Double_t* par)
{
  Double_t dMom = x[0];
  Double_t dSigmaT = par[0];

  Double_t dBetaMin = 0.8, dBetaMax = 1.2;
  TF1 *f1xBetaNDist = new TF1("f1xBetaNDist", betaNDist, dBetaMin, dBetaMax, 2);
  f1xBetaNDist->SetParameters(dSigmaT, dMom);
  f1xBetaNDist->SetNpx(1000);
  
  return (f1xBetaNDist->Integral(0.9, 1.6) * 100.); // * 100. for percentage
  //return(1.);
}


Double_t lostGammas(Double_t* x, Double_t* par)
{

  Double_t dSigmaT = x[0];

  TF1 *f1xBetaGDist = new TF1("f1xBetaGDist", betaGDist, 0., 1.2, 1);
  f1xBetaGDist->SetParameter(0, dSigmaT);
  f1xBetaGDist->SetNpx(1000);
  
  return (f1xBetaGDist->Integral(0.2, 0.9) * 100.);  // * 100. for percentage
  //return(1.);
}



TH1F* getBetaNDist(Double_t dSigmaT, Double_t dMom)
{
 
  Double_t dBetaMin = 0.8, dBetaMax = 1.2;
  TF1 *f1xBetaNDist = new TF1("f1xBetaNDist", betaNDist, dBetaMin, dBetaMax, 2);
  f1xBetaNDist->SetParameters(dSigmaT, dMom);
  f1xBetaNDist->SetParNames("#sigma_{t}", "p (neutron) in GeV");
  f1xBetaNDist->SetNpx(1000);
  f1xBetaNDist->Draw();

  TH1F* h1 = (TH1F*) f1xBetaNDist->GetHistogram();
  h1->SetLineWidth(0.8);

  return (h1);
}


TH1F* getBetaGDist(Double_t dSigmaT)
{
  
  TF1 *f1xBetaGDist = new TF1("f1xBetaGDist", betaGDist, 0.8, 1.2, 1);
  f1xBetaGDist->SetParameter(0, dSigmaT);
  f1xBetaGDist->SetParNames("#sigma_{t}");
  f1xBetaGDist->SetNpx(1000);
  f1xBetaGDist->Draw();
  
  TH1F* h1 = (TH1F*) f1xBetaGDist->GetHistogram();
  h1->SetLineWidth(0.8);

  return(h1);

}


TH1F* getLostNeutrons(Double_t dSigmaT)
{
  
  Double_t dMomMin = 1., dMomMax = 2.;
  TF1 *f1xLostN = new TF1("f1xLostN", lostNeutrons, dMomMin, dMomMax, 1);
  f1xLostN->SetParameter(0, dSigmaT);
  f1xLostN->SetNpx(1000);
  f1xLostN->Draw();

  TH1F* h1 = f1xLostN->GetHistogram();
  h1->SetLineWidth(0.8);

  return(h1);
}


TH1F* getLostGammas()
{
  
  f1xLostG = new TF1("f1xLostG", lostGammas, 0.4, 1., 0);
  f1xLostG->SetNpx(1000);
  f1xLostG->Draw();

  TH1F* h1 = f1xLostG->GetHistogram();
  h1->SetLineWidth(0.8);

  return(h1);
}



// -------------------------------------------------
// Functions  to call for plots in the documentation
// -------------------------------------------------

void drawBetaMom(Double_t dMomLimit)
{
  TF1 *f1xBeta = (TF1*) gDirectory->Get("f1xBeta");
  if (f1xBeta) delete f1xBeta;
  f1xBeta = new TF1("f1xBeta", betaNeutron, 0., dMomLimit, 0);
  f1xBeta->SetNpx(1000);
  f1xBeta->Draw();
  TH1F* h1 = (TH1F*) f1xBeta->GetHistogram()->Clone();
  
  h1->GetXaxis()->SetTitle("p (GeV/c)");
  h1->GetYaxis()->SetTitle("#beta");
  h1->GetYaxis()->CenterTitle();
  h1->SetTitle("#beta vs momentum for neutrons");
  h1->SetLineWidth(0.8);
  h1->Draw();
}


void drawBetaDist()
{

  Double_t dSigmaT1 = 0.4, dSigmaT2 = 0.8;
  Double_t dMom1 = 1., dMom2 = 1.5;

  Double_t dMax;
  char szComment[40];
  ostringstream s1, s2;
  s1.precision(2);
  s2.precision(2);
  s1 << dMom1 << " GeV/c neutron\n";
  s2 << dMom2 << " GeV/c neutron\n";

  TCanvas *c1 = new TCanvas("c1", "", 580, 230, 1015, 500); 
  c1->Divide(2,1);


  c1->cd(1);
  TH1F* h1 = (TH1F*) getBetaGDist(dSigmaT1)->Clone();
  h1->SetName("h1");
  TH1F* h2 = (TH1F*) getBetaNDist(dSigmaT1, dMom1)->Clone();
  h2->SetName("h2");
  TH1F* h3 = (TH1F*) getBetaNDist(dSigmaT1, dMom2)->Clone();
  h3->SetName("h3");

  int nSigmaT1 = floor(dSigmaT1 * 1000.);
  sprintf(szComment, "#beta distribution, time resolution %d ps", nSigmaT1);
  h1->SetTitle(szComment);
  h1->SetLineColor(2);
  h1->GetXaxis()->SetTitle("#beta");
  dMax = h1->GetMaximum();
  h2->SetLineColor(4);
  h3->SetLineColor(6);

  h1->Draw("C");
  h3->Draw("same");
  h2->Draw("same");

  TLine* line1 = new TLine(0.9, 0., 0.9, dMax);
  line1->SetLineWidth(2);
  line1->SetLineColor(1);
  line1->SetLineStyle(2);
  line1->Draw();

  TLegend *legend1 = new TLegend(0.550287, 0.625, 0.880747, 0.838983);
  legend1->AddEntry(h1, "#gamma", "l");
  legend1->AddEntry(h2, s1.str(), "l");
  legend1->AddEntry(h3, s2.str(), "l");
  legend1->AddEntry(line1, "#beta separation");
  legend1->Draw();

  c1->cd(2);
  TH1F* h4 = (TH1F*) getBetaGDist(dSigmaT2)->Clone();
  h3->SetName("h4");
  TH1F* h5 = (TH1F*) getBetaNDist(dSigmaT2, dMom1)->Clone();
  h5->SetName("h5");
  TH1F* h6 = (TH1F*) getBetaNDist(dSigmaT2, dMom2)->Clone();
  h6->SetName("h6");

  int nSigmaT2 = floor(dSigmaT2 * 1000.);
  sprintf(szComment, "#beta distribution, time resolution %d ps", nSigmaT2);
  h4->SetTitle(szComment);
  h4->SetLineColor(2);
  h4->GetXaxis()->SetTitle("#beta");
  dMax = h4->GetMaximum();
  h5->SetLineColor(4);
  h6->SetLineColor(6);

  h4->Draw("C");
  h6->Draw("same");
  h5->Draw("same");

  TLine* line2 = new TLine(0.9, 0., 0.9, dMax);
  line2->SetLineWidth(2);
  line2->SetLineColor(1);
  line2->SetLineStyle(2);
  line2->Draw();

  TLegend *legend2 = new TLegend(0.550287, 0.625, 0.880747, 0.838983);
  legend2->AddEntry(h4, "#gamma", "l");
  legend2->AddEntry(h5, s1.str(), "l");
  legend2->AddEntry(h6, s2.str(), "l");
  legend2->AddEntry(line2, "#beta separation");
  legend2->Draw();

}


void drawLostNeutrals()
{

  TCanvas *c1 = new TCanvas("c1", "", 580, 230, 1015, 500); 
  c1->Divide(2,1);

  c1->cd(1);
  TH1F* h1 = (TH1F*) getLostNeutrons(0.2)->Clone();  // 200 ps time resolution
  h1->SetName("h1");
  TH1F* h2 = (TH1F*) getLostNeutrons(0.4)->Clone();  // 400 ps time resolution
  h2->SetName("h2");                                 
  TH1F* h3 = (TH1F*) getLostNeutrons(0.6)->Clone();  // 600 ps time resolution
  h3->SetName("h3");
  TH1F* h4 = (TH1F*) getLostNeutrons(0.8)->Clone();  // 800 ps time resolution
  h4->SetName("h4");

  h4->SetTitle("Neutrons misidentified as #gamma");
  h4->GetXaxis()->SetTitle("neutron momentum (GeV/c)");
  h4->GetYaxis()->SetTitle("% identified as #gamma");
  h4->GetYaxis()->CenterTitle();
  h4->GetYaxis()->SetTitleOffset(1.1);
  h4->SetLineColor(2);
  h4->SetLineStyle(1);

  h2->SetLineColor(2);
  h3->SetLineColor(4);
  h4->SetLineColor(6);
  
  h4->Draw();
  h3->Draw("same");
  h2->Draw("same");
  h1->Draw("same");

  TLegend *legend = new TLegend(0.550287, 0.625, 0.880747, 0.838983);
  legend->AddEntry(h1, "#sigma_{t} = 200 ps", "l");
  legend->AddEntry(h2, "#sigma_{t} = 400 ps", "l");
  legend->AddEntry(h3, "#sigma_{t} = 600 ps", "l");
  legend->AddEntry(h4, "#sigma_{t} = 800 ps", "l");
  legend->Draw();
  
  c1->cd(2);
  
  TH1F *h5 = (TH1F*) getLostGammas()->Clone();
  h5->SetName("h5");
  h5->SetTitle("#gamma misidentified as neutrons");
  h5->GetXaxis()->SetTitle("#sigma_{t} (ns)");
  h5->GetYaxis()->SetTitle("% identified as neutrons");
  h5->GetYaxis()->CenterTitle();
  h5->GetYaxis()->SetTitleOffset(1.1);
  h5->Draw();
  
}



