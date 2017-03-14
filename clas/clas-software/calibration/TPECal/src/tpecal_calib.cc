// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "tpecal_calib.h"
#include "usage.h"

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TMath.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <cmath>
using namespace std;

// Class Constructor
tpecal_calib::tpecal_calib(tpecal_opts *opts)
{
	tpecalOpt = opts;
	string hd_msg = tpecalOpt->args["LOG_MSG"].args;
	cout << hd_msg << " Starting Run Index Table: " 
	               << tpecalOpt->args["RUN_INDEX"].args << endl;
	
	min_runno = (int) tpecalOpt->args["RUN_INDEX_MIN_RUN"].arg;
	if( min_runno != -1)
		cout << " Starting Run Index Min Run Number: " 
		     << tpecalOpt->args["RUN_INDEX_RUN"].arg << endl;
	max_runno = 99900;
	fit_all = 0;
	
	//cout << hd_msg << " Initializing Database..." << endl;
	//init_db();
	
	// Reset to Default FIT PARAMETERS
	reset_pars();
	
	Mean  = NULL;
	Meanu = NULL;
	for(int ss=0; ss<30;ss++)
	{
		TPECal_ADC[ss]         = NULL;
		TPECal_ADC_Single[ss]  = NULL;
		TPECal_TDC[ss]         = NULL;
		DEAD[ss]       = 0;
		MEAN[ss]       = 0;
		RMS[ss]        = 0;
		mean[ss]       = 0;
		emean[ss]      = 0;
		mip_value[ss]  = 0;
		mip_old[ss]    = 0;
		ped_value[ss]  = 0;
		ped_old[ss]    = 0;
		ped_sigma[ss]  = 0;
		ped_err[ss]    = 0;
		TPECal_HV_OLD[ss]  = 0;
		TPECal_HV_NEW[ss]  = 0;
		TPECal_HV_CORR[ss] = 9999;
	}
	
	
	open_tpecal_b   = false;
	open_mip_b      = false;
	open_ped_b      = false;
	processed_mip_b = false;
	hv_b            = false;
	
	if( tpecalOpt->args["ROOT_FILE"].args != "")
	{
		rfile = tpecalOpt->args["ROOT_FILE"].args;
		cout << " Opening ROOT File: " << rfile << endl;
		open_root();
	}
	
	// Functions to draw (more extended range than the following)
	BG  = new TF1("BG", backgroundf, 50, 700, 2);
	GS  = new TF1("GS", "gaus",      50, 600);
	PS  = new TF1("PS", poissonf,    50, 600, 3);
	BG->SetLineColor(8);
	BG->SetLineWidth(1);
	GS->SetLineColor(4);
	GS->SetLineWidth(1);
	PS->SetLineColor(4);
	PS->SetLineWidth(1);
	
	// Fit functions and limits
	Background = new TF1("Background", backgroundf,   50, 600, 2);
	Poisson    = new TF1("Poisson",    poissonf,     100, 650, 3);
	Gaussian   = new TF1("Gaussian",   gaussianf,    120, 400, 3);
	Fitf       = new TF1("Fitf",       fitf,          50, 700, 5);
	Fitf->SetLineColor(2);
	Fitf->SetLineWidth(1);
	
	PedSignal  = new TF1("PedSignal",  "gaus", 100, 800);
	PedSignal->SetLineColor(4);
	PedSignal->SetLineWidth(1);
	
}

// Functions Parameters Limits
void tpecal_calib::Set_ParLimits()
{
	Gaussian->SetParLimits(1, tpecalOpt->mean_min, tpecalOpt->mean_max);
	Gaussian->SetParLimits(2, tpecalOpt->sigm_min, tpecalOpt->sigm_max);
	Gaussian->SetParameters(1000, 4, 40);
	Poisson->SetParameters( 1000, 4, 40);
	
	Fitf->SetParameters(100, 0.1, 1000, 4, 40);
	Fitf->SetParLimits(0, 0, 1E9);
	Fitf->SetParLimits(1, 0, 1);
	Fitf->SetRange(tpecalOpt->args["MIN_FIT_RANGE"].arg, tpecalOpt->args["MAX_FIT_RANGE"].arg);
	
	PedSignal->SetRange(200, 600);
}

// Open ROOT Files
int tpecal_calib::open_root()
{
	int    BEAM   = (int) tpecalOpt->args["PROCESS_BEAM"].arg;
	datfile.append(rfile, 0, rfile.find(".root"));
	datfile.append(".dat");
	
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " ROOT I/O: >> " ;
	
	open_mip_b = false;
	open_ped_b = false;
	
	if(rfile.find("beam") != string::npos) open_mip_b = true;
	if(rfile.find("mip")  != string::npos) open_mip_b = true;
	if(rfile.find("ped")  != string::npos) open_ped_b = true;
	
	processed_mip_b = false;
	processed_ped_b = false;
	
	TH1F* RUNN;
	TH2F* MEANS;
	TH2F* EMEANS;
	TH3F* PARS;
	
	TFile *f = new TFile(rfile.c_str());
	if(!f) return 0;
	RUNN = (TH1F*)f->Get("runn");
	runno = (int) RUNN->GetBinContent(1);
	if(tpecalOpt->args["RUNNO"].arg != -1)
		runno = (int) tpecalOpt->args["RUNNO"].arg;
	if(!RUNN)
	{
		cout << hd_msg << " Something is wrong with " << rfile 
		               << "... maybe not a ROOT file, or not a TPE EC file?." << endl;
		return 0;
	}
	if(open_mip_b) read_mip_db();
	if(open_ped_b) read_ped_db();
	
	PARS    = (TH3F*)f->Get("pars");
	MEANS   = (TH2F*)f->Get("means");
	EMEANS  = (TH2F*)f->Get("emean");
	// make sure all histos are there
	if(!PARS || !MEANS || !EMEANS)
	{
		cout << hd_msg << " Something is wrong with " << rfile 
		     << ": not all histos are present." << endl;
		return 0;
	}
	for(int ss=0; ss<30;ss++)
	{
		if(TPECal_ADC[ss])         delete TPECal_ADC[ss];
		if(TPECal_ADC_Single[ss])  delete TPECal_ADC_Single[ss];
		if(TPECal_TDC[ss])         delete TPECal_TDC[ss];
		
		DEAD[ss] = 0;
		TPECal_ADC[ss]         = (TH1F*)f->Get(Form("Ch_%d",       ss+1));
		TPECal_ADC_Single[ss]  = (TH1F*)f->Get(Form("SCh_%d",      ss+1));
		TPECal_TDC[ss]         = (TH1F*)f->Get(Form("tdc_Ch_%d",   ss+1));
		TPECal_ADC[ss]->SetDirectory(0);
		TPECal_ADC_Single[ss]->SetDirectory(0);
		TPECal_TDC[ss]->SetDirectory(0);
		
		
		if(open_mip_b )
		{
//			TPECal_ADC[ss]->SetLabelOffset(.02, "XY");
//			TPECal_ADC[ss]->SetLabelFont(44, "XY");
//			TPECal_ADC[ss]->SetLabelSize(10, "XY");
//			TPECal_ADC[ss]->SetLabelColor(kBlack, "XY");
//			TPECal_ADC[ss]->SetNdivisions(505);
//			TPECal_ADC[ss]->SetTitleOffset(1.2);
//			TPECal_ADC[ss]->SetTitleSize(.052);
//			TPECal_ADC[ss]->SetXTitle(Form("ADC Ch %d   (L %d)  ", ss+1, ss/6+1 ));
//			TPECal_ADC[ss]->Sumw2();
//			TPECal_ADC[ss]->SetMinimum(0.00);
//			if(BEAM)
//			{
//				TPECal_ADC[ss]->SetLabelSize(8, "XY");
//				TPECal_ADC[ss]->SetTitleSize(.062);
//			}
//
//			TPECal_ADC_Single[ss]->SetLabelOffset(.02, "XY");
//			TPECal_ADC_Single[ss]->SetLabelFont(44, "XY");
//			TPECal_ADC_Single[ss]->SetLabelSize(10, "XY");
//			TPECal_ADC_Single[ss]->SetLabelColor(kBlack, "XY");
//			TPECal_ADC_Single[ss]->SetNdivisions(505);
//			TPECal_ADC_Single[ss]->SetTitleOffset(1.2);
//			TPECal_ADC_Single[ss]->SetTitleSize(.052);
//			TPECal_ADC_Single[ss]->SetXTitle(Form("ADC Ch %d   (L %d)  ", ss+1, ss/6+1 ));
//			TPECal_ADC_Single[ss]->Sumw2();
//			TPECal_ADC_Single[ss]->SetMinimum(0.00);
//			TPECal_ADC_Single[ss]->SetLineColor(kRed+2);
//			if(BEAM)
//			{
//				TPECal_ADC_Single[ss]->SetLabelSize(8, "XY");
//				TPECal_ADC_Single[ss]->SetTitleSize(.062);
//			}
//			
//						
//			TPECal_TDC[ss]->SetLabelOffset(.02, "XY");
//			TPECal_TDC[ss]->SetLabelFont(44, "XY");
//			TPECal_TDC[ss]->SetLabelSize(10, "XY");
//			TPECal_TDC[ss]->SetLabelColor(kBlack, "XY");
//			TPECal_TDC[ss]->SetNdivisions(505);
//			TPECal_TDC[ss]->SetTitleOffset(1.2);
//			TPECal_TDC[ss]->SetTitleSize(.052);
//			TPECal_TDC[ss]->SetXTitle(Form("TDC Ch %d   (L %d)  ", ss+1, ss/6+1 ));
//			TPECal_TDC[ss]->Sumw2();
//			TPECal_TDC[ss]->SetMinimum(0.00);
		}
		if(open_ped_b)
		{
			TPECal_ADC[ss]->SetLabelOffset(.02, "X");
			TPECal_ADC[ss]->SetLabelOffset(.02, "Y");
			TPECal_ADC[ss]->SetLabelFont(44, "XY");
			TPECal_ADC[ss]->SetLabelSize(7, "XY");
			TPECal_ADC[ss]->SetLabelColor(kBlack, "XY");
			TPECal_ADC[ss]->SetNdivisions(505);
			TPECal_ADC[ss]->SetTitleOffset(0.98);
			TPECal_ADC[ss]->SetTitleSize(.08);
			TPECal_ADC[ss]->SetXTitle(Form("ADC Ch %d   (L %d)  ", ss+1, ss/6+1 ));
			TPECal_ADC[ss]->Sumw2();
			TPECal_ADC[ss]->SetMinimum(0.);
		}
		
		MEAN[ss] = TPECal_ADC[ss]->GetMean();
		RMS[ss]  = TPECal_ADC[ss]->GetRMS();
		for(int p=0;p<5;p++)     parameters[ss][p] = 0;
		for(int p=0;p<3;p++) ped_parameters[ss][p] = 0;
		if(TPECal_ADC[ss]->Integral(1, 8200) < 10) { DEAD[ss]=1; }
		mip_value[ss] = MEANS->GetBinContent(ss);
		if(open_mip_b)
		{
			for(int p=0; p<5; p++) parameters[ss][p] = PARS->GetBinContent(ss, p);
			mean[ss]  = parameters[ss][3]*parameters[ss][4];
			emean[ss] = EMEANS->GetBinContent(ss);
		}
		if(open_ped_b)
		{
			for(int p=0; p<3; p++) ped_parameters[ss][p] = PARS->GetBinContent(ss, p);
			ped_value[ss] = ped_parameters[ss][1];
			ped_err[ss]   = EMEANS->GetBinContent(ss);
			ped_sigma[ss] = ped_parameters[ss][2];
			
		}
	}
	f->Close();
	double areparsthere = 0;   // if there are at least 20 fit in the file then should be around 4000
	for(int ss=0; ss<30;ss++)
		areparsthere += mean[ss];
	
	if(areparsthere>200 && open_mip_b) processed_mip_b = true;
	if(areparsthere>200 && open_ped_b) processed_ped_b = true;
	
	min_runno = runno;
	return runno;
}



void tpecal_calib::fit_spe(int ss)
{
	//Set_ParLimits();	
}

// Fits all spe channels
void tpecal_calib::fit_all_spe()
{
	fit_all = 1;
	for(int ss=0; ss<30; ss++)
		fit_spe(ss);
	processed_mip_b = true;
	fit_all = 0;
	write_pars();
}


// Builds(arg=1)/Delete(arg=0) SPE TGraphErrors from data
void tpecal_calib::spe_graphs(int opt)
{
	if(opt == 1)
	{
		double x[30],ex[30];
		for(int p=0; p<30; p++) {x[p] = p+1; ex[p] = 0;}
		double dead[30], modified[30];
		for(int ss=0; ss<30; ss++)
		{
			dead[ss]     = -99;
			modified[ss] = -99;
			
			if(DEAD[ss]) dead[ss] = 200;
			if(fabs(mean[ss] - mip_value[ss])>0.6 && !DEAD[ss]) modified[ss] =  mip_value[ss];
		}
		Meanu = new TGraphErrors(30, x, mip_value, ex, emean);
		MeanO = new TGraphErrors(30, x, mip_old,   ex, emean);
		Mean  = new TGraphErrors(30, x, mean,      ex, emean);
		MeanD = new TGraphErrors(30, x, dead,      ex, ex);
		MeanM = new TGraphErrors(30, x, modified,  ex, emean);
		
		Mean->SetMarkerColor(2);
		MeanO->SetMarkerColor(2);
		Meanu->SetMarkerColor(4);
		MeanD->SetMarkerColor(1);
		MeanM->SetMarkerColor(3);
		Mean->SetMarkerSize(0.7);
		MeanO->SetMarkerSize(0.7);
		Meanu->SetMarkerSize(0.75);
		MeanD->SetMarkerSize(0.75);
		MeanD->SetMarkerSize(0.75);
		Mean->SetMarkerStyle(21);
		MeanO->SetMarkerStyle(20);
		Meanu->SetMarkerStyle(21);
		MeanD->SetMarkerStyle(21);
		MeanM->SetMarkerStyle(21);		
	}
	else
	{
		delete Mean;
		delete Meanu;
		delete MeanO;
	}
}


// Builds(arg=1)/Delete(arg=0) PED TGraphErrors from data
void tpecal_calib::ped_graphs(int opt)
{
	if(opt == 1)
	{
		double x[30],ex[30];
		for(int p=0; p<30; p++) {x[p] = p+1; ex[p] = 0;}
		
		PMean   = new TGraphErrors(30, x, ped_value, ex, ped_err);
		PMeanO  = new TGraphErrors(30, x, ped_old,   ex, ped_err);

		PMeanO->SetMarkerColor(2);
		PMeanO->SetMarkerSize(0.78);
		PMeanO->SetMarkerStyle(21);
		PMean->SetMarkerColor(4);
		PMean->SetMarkerSize(0.81);
		PMean->SetMarkerStyle(21);
		
	}
	else
	{
		delete PMean;
		delete PMeanO;
	}
}

// Builds(arg=1)/Delete(arg=0) HV TGraphErrors from data
void tpecal_calib::hv_graphs(int opt)
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args + " HV >>";
	if(!hv_b)
	{
		cout << hd_msg << " Old HV values not loaded yet. Reading values from Database."
									 << " Override with .snap file if you want." <<  endl;
		read_hv_db();
	}
	double WANTED  = 300 ; // Wanted ch position of spe peak
	double E_CONST = 150 ; // Exponential constant : log[(V2/V1)^E_CONST] = WANTED / Measured
	
	if(hv_b && processed_mip_b)
	{
		for(int ss=0; ss<30; ss++)
		{
			TPECal_HV_NEW[ss]  =  TPECal_HV_OLD[ss] + (E_CONST * log(WANTED/mip_value[ss]));
			TPECal_HV_CORR[ss] =  TPECal_HV_NEW[ss] - TPECal_HV_OLD[ss];
		}
	}
	// update_table_display(2);
	if(!processed_mip_b)
	{
		cout << hd_msg << " Need to process SPE positions to calculate HV correction... " <<  endl;
	}
	
	if(opt == 1)
	{
		double x[30], ex[30];
		
		for(int p=0; p<30; p++) {x[p] = p+1; ex[p] = 0;}
		
		HVMeanO  = new TGraphErrors(30, x, TPECal_HV_OLD,  ex, ex);
		HVMeanN  = new TGraphErrors(30, x, TPECal_HV_NEW,  ex, ex);
		HVMeanD  = new TGraphErrors(30, x, TPECal_HV_CORR, ex, ex);
		HVMeanO->SetMarkerColor(2);
		HVMeanO->SetMarkerSize(0.8);
		HVMeanO->SetMarkerStyle(21);
		HVMeanN->SetMarkerColor(4);
		HVMeanN->SetMarkerSize(0.8);
		HVMeanN->SetMarkerStyle(21);
		HVMeanD->SetMarkerColor(1);
		HVMeanD->SetMarkerSize(0.8);
		HVMeanD->SetMarkerStyle(21);
		
	}
	else
	{
		delete HVMeanO;
		delete HVMeanN;
		delete HVMeanD;
	}
}





void tpecal_calib::write_pars()
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args + " FILE >>";
	cout << hd_msg << " Writing ROOT, dat files for run " << runno << "." << endl;
	
	TH2F* PARS;
	TH1F* MEANS;
	TH1F* EMEANS;
	TH1F* RUNN;
	
	TFile *f = new TFile(rfile.c_str(), "UPDATE");
	PARS    = (TH2F*)f->Get("pars");
	MEANS   = (TH1F*)f->Get("means");
	EMEANS  = (TH1F*)f->Get("emean");
	RUNN    = (TH1F*)f->Get("runn");
	for(int ss=0; ss<30;ss++)
	{
		MEANS->SetBinContent(  ss, mip_value[ss]);
		EMEANS->SetBinContent( ss,     emean[ss]);
		for(int p=0; p<5;p++) PARS->SetBinContent(ss, p, parameters[ss][p]);
	}
	
	RUNN->SetBinContent(1, runno);
	RUNN->Write();
	PARS->Write();
	MEANS->Write();
	EMEANS->Write();
	f->Close();
	
	ofstream outf(datfile.c_str(), ios::out);
	outf.precision(1);
	outf.setf(ios::fixed);
	
	for(int ss=0; ss<30;ss++)
	{
		outf  << "   " << ss + 1 << "  " ;
		if(ss<9) outf << " " ;
		outf << mip_value[ss] << "   " << emean[ss] << endl;
	}
	outf.close();
	
	// Writing out values of tubes below 170 and above 230
	string outf2 = datfile + "_notes";
	ofstream outff(outf2.c_str(), ios::out);
	outff.precision(1);
	outff.setf(ios::fixed);
	
	for(int ss=0; ss<30;ss++)
	{
		if(mip_value[ss] < 170 || mip_value[ss] > 230)
		{
			outff << "   " << ss + 1 << "  " ;
			if(ss<9) outff << " " ;
			outff << mip_value[ss] << "   " << emean[ss] <<  "   " << endl;
		}
	}
	outff.close();
	
	
	
}

void tpecal_calib::write_ped_pars()
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args + " FILE >>";
	cout << hd_msg << " Writing ROOT, dat files for run " << runno << "." << endl;
	
	TH2F* PARS;
	TH1F* MEANS;
	TH1F* EMEANS;
	
	TFile *f = new TFile(rfile.c_str(), "UPDATE");
	PARS    = (TH2F*)f->Get("pars");
	MEANS   = (TH1F*)f->Get("means");
	EMEANS  = (TH1F*)f->Get("emean");
	
	for(int ss=0; ss<30;ss++)
	{
		MEANS->SetBinContent( ss, ped_value[ss]);
		EMEANS->SetBinContent(ss, ped_err[ss]);
		for(int p=0; p<3;p++) PARS->SetBinContent(ss, p, ped_parameters[ss][p]);
	}
	
	PARS->Write();
	MEANS->Write();
	EMEANS->Write();
	f->Close();
	
	ofstream outf(datfile.c_str(), ios::out);
	outf.precision(1);
	outf.setf(ios::fixed);
	
	for(int ss=0; ss<30;ss++)
	{
		outf  << ss + 1 << "  " ;
		if(ss<9) outf << " " ;
		outf << ped_value[ss] << "   " << ped_err[ss] <<  "   " << ped_sigma[ss] << endl;
	}
	outf.close();
	
}


void tpecal_calib::fit_ped(int ss)
{
	double *err;
	double par[3];
	TPECal_ADC[ss]->Fit(PedSignal, "QNEM+");
	PedSignal->GetParameters(&par[0]);
	for(int p=0;p<3;p++) ped_parameters[ss][p] = par[p];  // Storing parameters
	TPECal_ADC[ss]->SetAxisRange(par[1]-20*par[2], par[1]+20*par[2]);
	
	err=PedSignal->GetParErrors();
	ped_value[ss] = par[1];
	ped_err[ss]   = err[1];
	ped_sigma[ss] = par[2];
}


// Fits all spe channels
void tpecal_calib::fit_all_ped()
{
	for(int ss=0; ss<30; ss++)
		fit_ped(ss);
	
	processed_ped_b = true;
	write_ped_pars();
}



void tpecal_calib::read_hv_snap(string filename)                 ///< Reads HV voltages from snap file
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args + " HV >>";
	ifstream DAT(filename.c_str(), ios::in);
	
	string tmp;
	
	float ftmp;
	float hv;
	int   s, ch;
	string LR;
	string burt;
	
	
	if(!DAT)
	{
		cout << hd_msg << " " << filename << " could not be opened." << endl;
		return;
	}
	else
	{
		cout << hd_msg << " Loading HV from " << filename <<  endl;
		while(burt.find("End BURT header") == string::npos)
		{
			burt.clear();
			char onel[400];
			DAT.getline(onel, 400);
			burt.append(onel);
			burt_header.push_back(burt);
		}		
		for(unsigned int i=0; i<burt_header.size(); i++)
			cout  << burt_header[i] << endl;
		DAT >> tmp;
		while(DAT)
		{
			if(tmp.find("DV") == 16)
			{
				s  = atoi(tmp.substr(9,1).c_str());
				LR = tmp.substr(14,1);
				ch = atoi(tmp.substr(11,2).c_str());
				DAT >> ftmp >> hv;
				int channel = ch*2;
				// converting from LR notation to 1-30 channel
				if(LR=="L") channel = ch*2 -1;
				if(LR=="R") channel = ch*2;
				// 				cout << "S " << s << " ch ";
				// 				cout.width(2);
				// 				cout << channel << " (" << LR;
				// 				cout.width(3);
				// 				cout << ch << ")    HV = " << hv << endl;
				
				TPECal_HV_OLD[channel-1]  = (int) hv ;
			}
			DAT >> tmp;
		}
		
		DAT.close();
		hv_b = true;
	}
}

// Notation: 1L = 1, 1R = 2, 2L = 3, 2R = 4 etc
void tpecal_calib::write_hv_snap()                 ///< Writes HV voltages to snap file
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args + " HV >>";
	string filename = Form("high_voltages_%d.snap", runno);
	ofstream DAT(filename.c_str(), ios::out);
	cout << hd_msg << " Writing HV to " << filename <<  endl;
	for(unsigned int j=0; j<burt_header.size(); j++)
		DAT << burt_header[j] << endl;
	
	for(int s=1; s<7; s++)
		for(int i=1; i<37; i++)
		{
			string STR =  "";
			string LR  =  "";
			string CH  =  "";
			int ch;
			if(i%2)
			{
				LR.append("L");
				ch = (i+1)/2;
				if(ch < 10) CH.append("0");
				CH.append(stringify(ch));
			}
			else  
			{
				LR.append("R");
				ch =  i/2;
				if(ch < 10) CH.append("0");
				CH.append(stringify(ch));
			}
			
			STR.append(Form("B_hv_TPECal_S%s_%s_DV 1  %e", CH.c_str(), LR.c_str(), TPECal_HV_NEW[i-1]));	
			
			
			DAT << STR << endl;
		}
	
	DAT.close();
}

void tpecal_calib::change_run_number(int which, int n)
{ 
	if(which == 0) min_runno = n;
	if(which == 1) max_runno = n;
}

// exponential with the power as parameter
double backgroundf(double *x, double *par)
{
	return par[0]*exp(-par[1]*(x[0]-50));
}

// gaussian
double gaussianf(double *x, double *par)
{
	return par[0]*exp(-0.5*pow((x[0]-par[1])/par[2], 2));
}

// gaussian
double gaussianff(double x, double *par)
{
	return par[0]*exp(-0.5*pow((x-par[1])/par[2], 2));
}

// general poisson distribution
// par[1] = L;
// par[2] = P;
// Mean   = LP;
double poissonf(double *x, double *par)
{
	double arg = 0;
	if (par[2] != 0) arg = x[0]/par[2];
	
	double arg2 = 0.;
	if (TMath::Gamma(arg+1) != 0) arg2 = pow(par[1], arg)/TMath::Gamma(arg+1);
	
	return par[0]*(arg2)*exp(-par[1]);
}

double poissonff(double x, double *par)
{
	double arg = 0;
	if (par[2] != 0) arg = x/par[2];
	
	double arg2 = 0.;
	if (TMath::Gamma(arg+1) != 0) arg2 = pow(par[1], arg)/TMath::Gamma(arg+1);
	
	return par[0]*(arg2)*exp(-par[1]);
}

// Background + Poisson is the fit function
double fitf(double *x, double *par)
{
	// return backgroundf(x, par) + gaussian(x, &par[2]);
	return backgroundf(x, par) + poissonf(x, &par[2]);
}


void tpecal_calib::reset_pars()
{
	tpecalOpt->mean_min = 120;
	tpecalOpt->mean_max = 500;
	tpecalOpt->sigm_min = 30;
	tpecalOpt->sigm_max = 300;
	tpecalOpt->back_min = (int) tpecalOpt->args["MIN_FIT_RANGE"].arg;
	tpecalOpt->back_max = (int) tpecalOpt->args["MAX_FIT_RANGE"].arg;
}


vector<int> tpecal_calib::neighbors(int ch)       ///< Return the vector of neighbours scintillators
{
	vector<int> neighbors;
	int row = jrow(ch);
	int col = jcol(ch);
	
	if(col != 0) neighbors.push_back(ch-1);
	if(col != 5) neighbors.push_back(ch+1);
	
	// row above
	if(row != 0)
	{
		neighbors.push_back(ch-6);  // one less row
		if(col != 0) neighbors.push_back(ch-7);
		if(col != 5) neighbors.push_back(ch-5);
	} 
	// row below
	if(row != 4)
	{
		neighbors.push_back(ch+6);  // one more row
		if(col != 0) neighbors.push_back(ch+5);
		if(col != 5) neighbors.push_back(ch+7);
	} 
	
//	for(int r=0; r<5; r++)
//	{
//		cout << endl;
//		for(int c=0; c<6; c++)
//		{
//			int cc = r*6+c + 1;
//			if( cc < 10) cout << " " ;
//			cout << "  " << cc ;
//			if(ch == cc-1) cout << " X ";
//			else           cout << " 0 ";
//		}
//	}
//	
//	cout << " Ch: " << ch << "  row: " << row << "  col: " << col << "  Neighbors: ";
//	for(unsigned int j=0; j<neighbors.size(); j++)
//		cout << " " << neighbors[j] + 1 ;
//	cout << endl << endl;
//	
	return neighbors;
}

