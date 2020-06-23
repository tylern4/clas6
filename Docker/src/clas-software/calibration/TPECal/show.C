void change_run()
{
	RUNN++;
	if(RUNN == NRUNS)
	RUNN = 0;
	cout << " Run Set is " << runs[RUNN]  << endl;
}

void print_all()
{
	PRINT = ".gif";
	for(int r=0; r<NRUNS; r++)
	{
		show_occupancies();
		change_run();
	}
	PRINT = "";
}

void show_occupancies()
{
	
	gStyle->SetPadLeftMargin(0.22);
	gStyle->SetPadRightMargin(0.02);
	gStyle->SetPadTopMargin(0.05);
	gStyle->SetPadBottomMargin(0.14);
	gStyle->SetFrameFillColor(kWhite);
	
	TLatex lab;
	lab.SetTextFont(102);
	lab.SetNDC();
	
	TCanvas *TPE = new TCanvas("TPE", "TPE", 1200, 800);
	lab.SetTextSize(0.032);
	lab.SetTextColor(kBlue+2);
	lab.DrawLatex(.1,.94, Form("%s", conf[RUNN].c_str() ));
//	lab.SetTextColor(kRed+2);
//	lab.DrawLatex(.1,.925, Form("%s", conf[RUNN+1].c_str() ));


	lab.SetTextColor(kBlack);
	lab.DrawLatex(.42,.03, Form("#leftarrow   [GeV]   #rightarrow") );
	
	
	TPad *TTPE   = new TPad("TTPE","TTPE", 0.01, 0.06, 0.99, 0.90);
	TTPE->Draw();
	TTPE->Divide(6, 5);
	
	

	lab.SetTextFont(52);
	lab.SetTextSize(0.13);
	lab.SetTextColor(kBlack);
	for(int c=0; c<30; c++)
	{
		TTPE->cd(c+1);
		occu[c][RUNN]->SetLineColor(kBlue+2);
		occu[c][RUNN]->SetLineWidth(1.5);
		occu[c][RUNN]->Draw("hist");
//		occu[c][RUNN+1]->SetLineColor(kRed+2);
//		occu[c][RUNN+1]->SetLineWidth(1.5);
//		occu[c][RUNN+1]->Draw("histsame");


		lab.DrawLatex(.55,.82, Form("Ch %d", c+1));
		lab.DrawLatex(.55,.68, Form("N: %3.1fK", occu[c][RUNN]->Integral()/1000.0));

	}

	if(PRINT != "")
	TPE->Print(Form("tpe_runs_%3.2f_%d%s", tpos[RUNN], runs[RUNN], PRINT.c_str()));

}




