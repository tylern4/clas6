{
	const int NRUNS = 11;
	
	// for each run, door all open and door 1/2 closed
	int    runs[NRUNS] = {66247, 66248, 66262, 66250, 66260, 66252, 66259, 66254, 66263, 66257, 66256};
	double tpos[NRUNS] = { 2.13,  2.32,  2.51,  2.70,  2.89,  3.08,  3.27,  3.46,  3.65,  3.76,  3.84};
												
	string conf[NRUNS] = {"66247: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 2.13", 
		                    "66248: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 2.32",
	                    	"66262: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 2.51",
	                    	"66250: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 2.70",
	                    	"66260: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 2.89",
	                    	"66252: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 3.08",
	                    	"66259: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 3.27",
	                    	"66254: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 3.46",
	                    	"66263: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 3.65",
	                    	"66257: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 3.76",
		                    "66256: Beam Right Blocked, Left 1/2 Blocked, TPECal x = 3.84"};
	
	
	int RUNN      = 0;
	string PRINT  = "";
	
	TH1F *occu[30][NRUNS];

	
	for(int r=0; r<NRUNS; r++)
	{
		TFile f(Form("tpe_beam_%d.root", runs[r]));
		for(int c=0; c<30; c++)
		{
			occu[c][r] = (TH1F*) f.Get(Form("Ch_%d", c+1)); 
			occu[c][r]->SetLabelOffset(.03, "XY");
			occu[c][r]->SetLabelFont(42, "XY");
			occu[c][r]->SetLabelSize(.11, "XY");
			occu[c][r]->SetLabelColor(kBlack, "XY");
			occu[c][r]->SetNdivisions(505, "XY");
			occu[c][r]->SetTitleOffset(1.);
			occu[c][r]->SetTitleSize(.11);
			occu[c][r]->SetXTitle(Form("Ch %d ", c+1));
			occu[c][r]->SetMinimum(0.00);
			occu[c][r]->SetDirectory(0);
		}
		f.Close();
	}


	gROOT->LoadMacro("show.C");
	show_occupancies();

	bar = new TControlBar("vertical", "TPE Histos");
	bar->AddButton("","");
	bar->AddButton("Show Occupancies", "show_occupancies()");
	bar->AddButton("Change Run Set",   "change_run()");
	bar->AddButton("","");
	bar->AddButton("Print All",        "print_all()");
	bar->AddButton("","");
	bar->Show();
 	

}
