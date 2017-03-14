{
	gStyle->SetPadLeftMargin(0.14);
	gStyle->SetPadRightMargin(0.1);
	gStyle->SetPadTopMargin(0.1);
	gStyle->SetPadBottomMargin(0.15);
	
	double s2ch1[5];
	double s2ch2[5];
	double s2ch3[5];
	double s2ch4[5];
	double thr[5];
	double runno[5];
	
	string tmp;
	ifstream IN("threshold.txt", ios::in);
	IN >> tmp >> tmp >> tmp >> tmp >> tmp >> tmp;
	for(int t=0; t<5; t++)
		IN >> runno[t] >> thr[t] >> s2ch1[t] >> s2ch2[t] >> s2ch3[t] >> s2ch4[t];


	TGraph *ch1g = new TGraph(5, thr, s2ch1);
	TGraph *ch2g = new TGraph(5, thr, s2ch2);
	TGraph *ch3g = new TGraph(5, thr, s2ch3);
	TGraph *ch4g = new TGraph(5, thr, s2ch4);
	
	ch1g->SetMarkerStyle(21);
	ch2g->SetMarkerStyle(20);
	ch3g->SetMarkerStyle(22);
	ch4g->SetMarkerStyle(23);
	
	ch1g->SetMarkerColor(1);
	ch2g->SetMarkerColor(2);
	ch3g->SetMarkerColor(3);
	ch4g->SetMarkerColor(4);
	

	ch1g->Draw("AP");
	ch2g->Draw("Psame");
	ch3g->Draw("Psame");
	ch4g->Draw("Psame");
	
	ch1g->GetXaxis()->SetTitle("threshold   (mV)");
	ch1g->GetYaxis()->SetTitle("threshold   ADC");
	ch1g->GetXaxis()->SetTitleOffset(1.5);
	ch1g->GetYaxis()->SetTitleOffset(1.5);
	
	
	TLine spe(10, 200, 110, 200);
	spe.Draw("same");

	TLatex lab;
	lab.SetNDC();
	lab.SetTextSize(0.04);
	lab.DrawLatex(0.53, 0.4, "#uparrow SPE calibrated to 200 ADC");

	TLegend *channels = new TLegend(0.18,  0.6,  0.4,  0.86);
	channels->AddEntry(ch1g,      "S2 ch1",      "p");
	channels->AddEntry(ch2g,      "S2 ch2",      "p");
	channels->AddEntry(ch3g,      "S2 ch3",      "p");
	channels->AddEntry(ch4g,      "S2 ch4",      "p");
	channels->Draw();
}








