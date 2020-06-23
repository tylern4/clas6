// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "cc_calib.h"
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

cc_calib::cc_calib(cc_opts *opts)
{
	ccOpt = opts;
	string hd_msg = ccOpt->args["LOG_MSG"].args;
	cout << hd_msg << " Starting Run Index Table: " << ccOpt->args["RUN_INDEX"].args << endl;

	min_runno = (int) ccOpt->args["RUN_INDEX_MIN_RUN"].arg;
	if( min_runno != -1)
		cout << " Starting Run Index Min Run Number: " << ccOpt->args["RUN_INDEX_RUN"].arg << endl;
	max_runno = 99900;
	fit_all = 0;

	cout << hd_msg << " Initializing Database..." << endl;
	init_db();

	// Reset to Default FIT PARAMETERS
	reset_pars();

	for(int s=0; s<6;s++)
	{
		Mean[s]  = NULL;
		Meanu[s] = NULL;
		for(int ss=0; ss<36;ss++)
		{
			CC_ADC[s][ss]     = NULL;
			DEAD[s][ss]       = 0;
			MEAN[s][ss]       = 0;
			RMS[s][ss]        = 0;
			mean[s][ss]       = 0;
			emean[s][ss]      = 0;
			poissMu[s][ss]    = 0;
			poissP[s][ss]     = 0;
			spe_value[s][ss]  = 0;
			spe_old[s][ss]    = 0;
			ped_value[s][ss]  = 0;
			ped_old[s][ss]    = 0;
			ped_sigma[s][ss]  = 0;
			ped_err[s][ss]    = 0;
			CC_HV_OLD[s][ss]  = 0;
			CC_HV_NEW[s][ss]  = 0;
			CC_HV_CORR[s][ss] = 9999;
		}
	}

	open_cc_b       = false;
	open_spe_b      = false;
	open_ped_b      = false;
	processed_spe_b = false;
	hv_b            = false;

	if( ccOpt->args["ROOT_FILE"].args != "")
	{
		rfile = ccOpt->args["ROOT_FILE"].args;
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

void cc_calib::Set_ParLimits()
{
	Gaussian->SetParLimits(1, ccOpt->mean_min, ccOpt->mean_max);
	Gaussian->SetParLimits(2, ccOpt->sigm_min, ccOpt->sigm_max);
	Gaussian->SetParameters(1000, 4, 40);
	Poisson->SetParameters( 1000, 4, 40);

	Fitf->SetParameters(100, 0.1, 1000, 4, 40);
	Fitf->SetParLimits(0, 0, 1E9);
	Fitf->SetParLimits(1, 0, 1);
	Fitf->SetRange(ccOpt->args["MIN_FIT_RANGE"].arg, ccOpt->args["MAX_FIT_RANGE"].arg);

	PedSignal->SetRange(100, 800);
}

int cc_calib::open_root()
{
	datfile.append(rfile, 0, rfile.find(".root"));
	datfile.append(".dat");

	string hd_msg    = ccOpt->args["LOG_MSG"].args + " ROOT I/O: >> " ;

	open_spe_b = false;
	open_ped_b = false;

	if(rfile.find("spe") != string::npos) open_spe_b = true;
	if(rfile.find("ped") != string::npos) open_ped_b = true;

	processed_spe_b = false;
	processed_ped_b = false;

	TH1F* RUNN;
	TH2F* MEANS;
	TH2F* EMEANS;
	TH2F* CHI2S;
	TH2F* POISMUS;
	TH2F* POISPS;
	TH3F* PARS;
	TFile *f = new TFile(rfile.c_str());
	if(!f) return 0;
	RUNN = (TH1F*)f->Get("runn");
	runno = (int) RUNN->GetBinContent(1);
	if(ccOpt->args["RUNNO"].arg != -1)
		runno = (int) ccOpt->args["RUNNO"].arg;
	if(!RUNN)
	{
		cout << hd_msg << " Something is wrong with " << rfile << "... maybe not a ROOT file, or not a CC file?." << endl;
		return 0;
	}
	if(open_spe_b) read_spe_db();
	if(open_ped_b) read_ped_db();

	PARS    = (TH3F*)f->Get("pars");
	MEANS   = (TH2F*)f->Get("means");
	EMEANS  = (TH2F*)f->Get("emean");
	CHI2S   = (TH2F*)f->Get("chi2s");
	POISMUS = (TH2F*)f->Get("poissMus");
	POISPS  = (TH2F*)f->Get("poissPs");
	// make sure all histos are there
	if(!PARS || !MEANS || !EMEANS || !CHI2S || !POISMUS || !POISPS)
	{
		cout << hd_msg << " Something is wrong with " << rfile << "not all histos are present." << endl;
		return 0;
	}
	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
		{
			if(CC_ADC[s][ss]) delete CC_ADC[s][ss];
			DEAD[s][ss] = 0;
			CC_ADC[s][ss] = (TH1F*)f->Get(Form("Sect_%d__segm_%d",s+1, ss+1));
			if(open_spe_b) CC_ADC[s][ss]->SetAxisRange(50,  700);
			if(open_ped_b) CC_ADC[s][ss]->SetAxisRange(100, 800);

			CC_ADC[s][ss]->SetLabelOffset(.02, "XY");
			CC_ADC[s][ss]->SetLabelFont(44, "XY");
			CC_ADC[s][ss]->SetLabelSize(12, "XY");
			CC_ADC[s][ss]->SetNdivisions(505);
			CC_ADC[s][ss]->SetTitleOffset(1.0);
			CC_ADC[s][ss]->SetTitleSize(.052);
			CC_ADC[s][ss]->SetXTitle(Form("Sector %d  -  Ch %d     (%s)      ", s+1, ss+1, cc_nomenclature(ss).c_str()));
			CC_ADC[s][ss]->Sumw2();
			CC_ADC[s][ss]->SetMinimum(0);
			CC_ADC[s][ss]->SetDirectory(0);

			MEAN[s][ss] = CC_ADC[s][ss]->GetMean();
			RMS[s][ss]  = CC_ADC[s][ss]->GetRMS();
			for(int p=0;p<5;p++)     parameters[s][ss][p] = 0;
			for(int p=0;p<3;p++) ped_parameters[s][ss][p] = 0;
			if(CC_ADC[s][ss]->Integral(60, 400) < 2000) { DEAD[s][ss]=1; }
			spe_value[s][ss] = MEANS->GetBinContent(s, ss);
			if(open_spe_b)
			{
				for(int p=0; p<5; p++) parameters[s][ss][p] = PARS->GetBinContent(s, ss, p);
				mean[s][ss]  = parameters[s][ss][3]*parameters[s][ss][4];
				emean[s][ss] = EMEANS->GetBinContent(s, ss);
				if(CHI2S)
				{
					chi2df[s][ss]  = CHI2S->GetBinContent(s, ss);
					poissMu[s][ss] = POISMUS->GetBinContent(s, ss);
					poissP[s][ss]  = POISPS->GetBinContent(s, ss);
				}
			}
			if(open_ped_b)
			{
				for(int p=0; p<3; p++) ped_parameters[s][ss][p] = PARS->GetBinContent(s, ss, p);
				ped_value[s][ss] = ped_parameters[s][ss][1];
				ped_err[s][ss]   = EMEANS->GetBinContent(s, ss);
				ped_sigma[s][ss] = ped_parameters[s][ss][2];
				// CC_ADC[s][ss]->SetAxisRange(ped_parameters[s][ss][1]-20*ped_parameters[s][ss][2], ped_parameters[s][ss][1]+20*ped_parameters[s][ss][2]);

			}
		}
	f->Close();
	double areparsthere = 0;   // if there are at least 20 fit in the file then should be around 4000
	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
			areparsthere += mean[s][ss];

	if(areparsthere>2000 && open_spe_b) processed_spe_b = true;
	if(areparsthere>2000 && open_ped_b) processed_ped_b = true;

	min_runno = runno;
	return runno;
}



void cc_calib::fit_spe(int s, int ss)
{
	Set_ParLimits();
	double min, max;
	double par[5] = {0,0,0,0,0};
	double *err;

	double nbins = CC_ADC[s][ss]->GetNbinsX();
	double xmin  = CC_ADC[s][ss]->GetXaxis()->GetXmin();
	double xmax  = CC_ADC[s][ss]->GetXaxis()->GetXmax();
	double chbin = (xmax - xmin)/nbins;  // ADCs/bin

	//  First constraint on Background parameters:
	//  at ADC=50 must be equal or less then the average of the first
	//  "navg" bins of the histo ( = 50 ADC)
	max = 0;
	int navg = 25;
	for(int c=0; c<navg; c++) max += CC_ADC[s][ss]->GetBinContent(c + CC_ADC[s][ss]->FindBin(50))/navg;

	//  Second constraint on Background parameters:
	//  at ADC=600, exponential background must be less than the tail
	//  averaged at the bins of the histo from 500 to 700
	min = 0;
	int bin_adc500 = CC_ADC[s][ss]->FindBin(500);
	int bin_adc700 = CC_ADC[s][ss]->FindBin(700);
	navg =  bin_adc700 - bin_adc500;
	for(int c=bin_adc500; c<bin_adc700; c++) min += CC_ADC[s][ss]->GetBinContent(c)/navg;

	if(!DEAD[s][ss])
	{
		double left, right;
		double ileft, iright;
		left  = ccOpt->back_min;
		right = ccOpt->back_max;
		double ndf;
		chi2df[s][ss] = 100;

		// more precise for individual fit 
		double left_iterations  = 1;
		double right_iterations = 1;
		if(!fit_all)
		{
			left_iterations  = 4;
			right_iterations = 3;
		}
		double left_bin_size  = 8;
		double right_bin_size = 40;

		double backtosignal;
		//  Minimizing Chi2 looping over:
		//  fit range left side, from 50 to 75
		//  fit range right side, from 440 to 640
		for(int l=0; l<left_iterations; l++)
			for(int r=0; r<right_iterations; r++)
			{
				left  = ccOpt->back_min + l*left_bin_size;
				right = ccOpt->back_max - r*right_bin_size;
				ndf   = (right-left)/chbin - 5;
				Fitf->SetParLimits(0, 0, max);
				Fitf->SetParLimits(1, -log(min/max)/(600-50), 0.3);
				Fitf->SetParLimits(3, 0.1, 20.0);
				Fitf->SetParLimits(4, 5.0, 200.0);

				// first fit
				CC_ADC[s][ss]->Fit("Fitf" ,"QREMN", "", left, right);
				// check how much of background on the left is due to poisson or exponential... adjust
				backtosignal = max - poissonff(70, &par[2]);
				// cout << " Poisson at 70: " <<  poissonff(70, &par[2]) << ", background at 70: " << max << " Background at 650: " << min << endl;
				if(backtosignal < 0) backtosignal = 10;
				Fitf->SetParLimits(0, 0, backtosignal);
				//CC_ADC[s][ss]->SetAxisRange(left, right);
				CC_ADC[s][ss]->Fit("Fitf" ,"QREMN", "", left, right);

				if(Fitf->GetChisquare()/ndf < chi2df[s][ss])
				{
					chi2df[s][ss]  = Fitf->GetChisquare()/ndf;
					ileft          = l;
					iright         = r;
					Fitf->GetParameters(&par[0]);
				}
			}
		
		cout <<  " Sector " << s+1 << " - Channel " << ss+1 
				<< "  left: " << left << " right: " << right << " chi2: " << chi2df[s][ss]
				<< " Signal at left: " << poissonff(left, &par[2]) << endl;

		for(int p=0;p<5;p++) parameters[s][ss][p] = par[p];  // Storing parameters

		err              = Fitf->GetParErrors();
		mean[s][ss]      = par[3]*par[4];
		emean[s][ss]     = mean[s][ss]*sqrt(pow(err[3]/par[3], 2) + pow(err[4]/par[4], 2));
		spe_value[s][ss] = par[3]*par[4];
		poissMu[s][ss]   = par[3];
		poissP[s][ss]    = par[4];
	}
}

// Fits all spe channels
void cc_calib::fit_all_spe()
{
	fit_all = 1;
	for(int s=0; s<6; s++)
		for(int ss=0; ss<36; ss++)
			fit_spe(s,ss);
	processed_spe_b = true;
	fit_all = 0;
	write_pars();
}


// Builds(arg=1)/Delete(arg=0) SPE TGraphErrors from data
void cc_calib::spe_graphs(int opt)
{
	if(opt == 1)
	{
		double x[36],ex[36];
		for(int p=0; p<36; p++) {x[p] = p+1; ex[p] = 0;}
		double dead[6][36], modified[6][36];
		for(int s=0; s<6;  s++)
			for(int ss=0; ss<36; ss++)
		{
			dead[s][ss]     = -99;
			modified[s][ss] = -99;

			if(DEAD[s][ss]) dead[s][ss] = 200;
			if(fabs(mean[s][ss] - spe_value[s][ss])>0.6 && !DEAD[s][ss]) modified[s][ss] =  spe_value[s][ss];
		}
		for(int s=0; s<6;  s++)  Meanu[s] = new TGraphErrors(36, x, spe_value[s], ex, emean[s]);
		for(int s=0; s<6;  s++)  MeanO[s] = new TGraphErrors(36, x, spe_old[s],   ex, emean[s]);
		for(int s=0; s<6;  s++)  Mean[s]  = new TGraphErrors(36, x, mean[s],      ex, emean[s]);
		for(int s=0; s<6;  s++)  MeanD[s] = new TGraphErrors(36, x, dead[s],      ex, ex);
		for(int s=0; s<6;  s++)  MeanM[s] = new TGraphErrors(36, x, modified[s],  ex, emean[s]);

		for(int s=0; s<6; s++)
		{
			Mean[s]->SetMarkerColor(2);
			MeanO[s]->SetMarkerColor(2);
			Meanu[s]->SetMarkerColor(4);
			MeanD[s]->SetMarkerColor(1);
			MeanM[s]->SetMarkerColor(3);
			Mean[s]->SetMarkerSize(0.7);
			MeanO[s]->SetMarkerSize(0.7);
			Meanu[s]->SetMarkerSize(0.75);
			MeanD[s]->SetMarkerSize(0.75);
			MeanD[s]->SetMarkerSize(0.75);
			Mean[s]->SetMarkerStyle(21);
			MeanO[s]->SetMarkerStyle(20);
			Meanu[s]->SetMarkerStyle(21);
			MeanD[s]->SetMarkerStyle(21);
			MeanM[s]->SetMarkerStyle(21);
		}
	}
	else
	{
		for(int s=0; s<6; s++) delete Mean[s];
		for(int s=0; s<6; s++) delete Meanu[s];
		for(int s=0; s<6; s++) delete MeanO[s];
	}
}


// Builds(arg=1)/Delete(arg=0) PED TGraphErrors from data
void cc_calib::ped_graphs(int opt)
{
	if(opt == 1)
	{
		double x[36],ex[36];
		for(int p=0; p<36; p++) {x[p] = p+1; ex[p] = 0;}

		for(int s=0; s<6;  s++)  PMean[s]   = new TGraphErrors(36, x, ped_value[s], ex, ped_err[s]);
		for(int s=0; s<6;  s++)  PMeanO[s]  = new TGraphErrors(36, x, ped_old[s],   ex, ped_err[s]);
		for(int s=0; s<6; s++)
		{
			PMeanO[s]->SetMarkerColor(2);
			PMeanO[s]->SetMarkerSize(0.78);
			PMeanO[s]->SetMarkerStyle(21);
			PMean[s]->SetMarkerColor(4);
			PMean[s]->SetMarkerSize(0.81);
			PMean[s]->SetMarkerStyle(21);
		}
	}
	else
	{
		for(int s=0; s<6; s++) delete PMean[s];
		for(int s=0; s<6; s++) delete PMeanO[s];
	}
}

// Builds(arg=1)/Delete(arg=0) HV TGraphErrors from data
// Notation: 1L = 1, 1R = 2, 2L = 3, 2R = 4 etc
void cc_calib::hv_graphs(int opt)
{
	string hd_msg = ccOpt->args["LOG_MSG"].args + " HV >>";
	if(!hv_b)
	{
		cout << hd_msg << " Old HV values not loaded yet. Reading values from Database. Override with .snap file if you want." <<  endl;
		read_hv_db();
	}
	double WANTED  = 200 ; // Wanted ch position of spe peak
	double E_CONST = 150 ; // Exponential constant : log[(V2/V1)^E_CONST] = WANTED / Measured

	if(hv_b && processed_spe_b)
	{
		for(int s=0; s<6; s++) for(int ss=0; ss<36; ss++)
		{
			CC_HV_NEW[s][ss]  =  CC_HV_OLD[s][ss] + (E_CONST * log(WANTED/spe_value[s][ss]));
			CC_HV_CORR[s][ss] =  CC_HV_NEW[s][ss] - CC_HV_OLD[s][ss];
		}
	}
	// update_table_display(2);
	if(!processed_spe_b)
	{
		cout << hd_msg << " Need to process SPE positions to calculate HV correction... " <<  endl;
	}

	if(opt == 1)
	{
		double x[36], ex[36];

		for(int p=0; p<36; p++) {x[p] = p+1; ex[p] = 0;}



		for(int s=0; s<6;  s++)  HVMeanO[s]  = new TGraphErrors(36, x, CC_HV_OLD[s],  ex, ex);
		for(int s=0; s<6;  s++)  HVMeanN[s]  = new TGraphErrors(36, x, CC_HV_NEW[s],  ex, ex);
		for(int s=0; s<6;  s++)  HVMeanD[s]  = new TGraphErrors(36, x, CC_HV_CORR[s], ex, ex);
		for(int s=0; s<6; s++)
		{
			HVMeanO[s]->SetMarkerColor(2);
			HVMeanO[s]->SetMarkerSize(0.8);
			HVMeanO[s]->SetMarkerStyle(21);
			HVMeanN[s]->SetMarkerColor(4);
			HVMeanN[s]->SetMarkerSize(0.8);
			HVMeanN[s]->SetMarkerStyle(21);
			HVMeanD[s]->SetMarkerColor(1);
			HVMeanD[s]->SetMarkerSize(0.8);
			HVMeanD[s]->SetMarkerStyle(21);
		}
	}
	else
	{
		for(int s=0; s<6; s++) delete HVMeanO[s];
		for(int s=0; s<6; s++) delete HVMeanN[s];
		for(int s=0; s<6; s++) delete HVMeanD[s];
	}
}





void cc_calib::write_pars()
{
	string hd_msg = ccOpt->args["LOG_MSG"].args + " FILE >>";
	cout << hd_msg << " Writing ROOT, dat files for run " << runno << "." << endl;

	TH3F* PARS;
	TH2F* MEANS;
	TH2F* EMEANS;
	TH2F* CHI2S;
	TH2F* POISMUS;
	TH2F* POISPS;
	TH1F* RUNN;

	TFile *f = new TFile(rfile.c_str(), "UPDATE");
	PARS    = (TH3F*)f->Get("pars");
	MEANS   = (TH2F*)f->Get("means");
	EMEANS  = (TH2F*)f->Get("emean");
	CHI2S   = (TH2F*)f->Get("chi2s");
	POISMUS = (TH2F*)f->Get("poissMus");
	POISPS  = (TH2F*)f->Get("poissPs");
	RUNN    = (TH1F*)f->Get("runn");
	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
		{
			MEANS->SetBinContent(  s, ss, spe_value[s][ss]);
			EMEANS->SetBinContent( s, ss,     emean[s][ss]);
			CHI2S->SetBinContent(  s, ss,    chi2df[s][ss]);
			POISMUS->SetBinContent(s, ss,   poissMu[s][ss]);
			POISPS->SetBinContent( s, ss,    poissP[s][ss]);
			for(int p=0; p<5;p++) PARS->SetBinContent(s, ss, p, parameters[s][ss][p]);
		}

	RUNN->SetBinContent(1, runno);
	RUNN->Write();
	PARS->Write();
	MEANS->Write();
	EMEANS->Write();
	CHI2S->Write();
	POISMUS->Write();
	POISPS->Write();
	f->Close();

	ofstream outf(datfile.c_str(), ios::out);
	outf.precision(1);
	outf.setf(ios::fixed);

	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
	{
		outf << s + 1 << "   " << ss + 1 << "  " ;
		if(ss<9) outf << " " ;
		outf << spe_value[s][ss] << "   " << emean[s][ss] <<  "   " << chi2df[s][ss] << endl;
	}
	outf.close();

	// Writing out values of tubes below 170 and above 230
	string outf2 = datfile + "_notes";
	ofstream outff(outf2.c_str(), ios::out);
	outff.precision(1);
	outff.setf(ios::fixed);

	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
		{
			if(spe_value[s][ss] < 170 || spe_value[s][ss] > 230)
			{
				outff << s + 1 << "   " << ss + 1 << "  " ;
				if(ss<9) outff << " " ;
				outff << spe_value[s][ss] << "   " << emean[s][ss] <<  "   " << chi2df[s][ss] << endl;
			}
		}
	outff.close();
 
 
 
}

void cc_calib::write_ped_pars()
{
	string hd_msg = ccOpt->args["LOG_MSG"].args + " FILE >>";
	cout << hd_msg << " Writing ROOT, dat files for run " << runno << "." << endl;

	TH3F* PARS;
	TH2F* MEANS;
	TH2F* EMEANS;

	TFile *f = new TFile(rfile.c_str(), "UPDATE");
	PARS    = (TH3F*)f->Get("pars");
	MEANS   = (TH2F*)f->Get("means");
	EMEANS  = (TH2F*)f->Get("emean");

	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
		{
			MEANS->SetBinContent( s, ss, ped_value[s][ss]);
			EMEANS->SetBinContent(s, ss, ped_err[s][ss]);
			for(int p=0; p<3;p++) PARS->SetBinContent(s, ss, p, ped_parameters[s][ss][p]);
		}

	PARS->Write();
	MEANS->Write();
	EMEANS->Write();
	f->Close();

	ofstream outf(datfile.c_str(), ios::out);
	outf.precision(1);
	outf.setf(ios::fixed);

	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
		{
			outf << s + 1 << "   " << ss + 1 << "  " ;
			if(ss<9) outf << " " ;
			outf << ped_value[s][ss] << "   " << ped_err[s][ss] <<  "   " << ped_sigma[s][ss] << endl;
		}
	outf.close();

}


void cc_calib::fit_ped(int s, int ss)
{
	double *err;
	double par[3];
	CC_ADC[s][ss]->Fit(PedSignal, "QNEM+");
	PedSignal->GetParameters(&par[0]);
	for(int p=0;p<3;p++) ped_parameters[s][ss][p] = par[p];  // Storing parameters
	CC_ADC[s][ss]->SetAxisRange(par[1]-20*par[2], par[1]+20*par[2]);

	err=PedSignal->GetParErrors();
	ped_value[s][ss] = par[1];
	ped_err[s][ss]   = err[1];
	ped_sigma[s][ss] = par[2];
}


// Fits all spe channels
void cc_calib::fit_all_ped()
{
	for(int s=0; s<6; s++)
		for(int ss=0; ss<36; ss++)
			fit_ped(s,ss);
	
	processed_ped_b = true;
	write_ped_pars();
}



// Notation: 1L = 1, 1R = 2, 2L = 3, 2R = 4 etc
void cc_calib::read_hv_snap(string filename)                 ///< Reads HV voltages from snap file
{
	string hd_msg = ccOpt->args["LOG_MSG"].args + " HV >>";
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
				// converting from LR notation to 1-36 channel
				if(LR=="L") channel = ch*2 -1;
				if(LR=="R") channel = ch*2;
// 				cout << "S " << s << " ch ";
// 				cout.width(2);
// 				cout << channel << " (" << LR;
// 				cout.width(3);
// 				cout << ch << ")    HV = " << hv << endl;
				
				CC_HV_OLD[s-1][channel-1]  = (int) hv ;
			}
			DAT >> tmp;
		}

		DAT.close();
		hv_b = true;
	}
}

// Notation: 1L = 1, 1R = 2, 2L = 3, 2R = 4 etc
void cc_calib::write_hv_snap()                 ///< Writes HV voltages to snap file
{
	string hd_msg = ccOpt->args["LOG_MSG"].args + " HV >>";
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
			
			STR.append(Form("B_hv_CC_S%d_%s_%s_DV 1  %e", s, CH.c_str(), LR.c_str(), CC_HV_NEW[s-1][i-1]));	
			
				
			DAT << STR << endl;
		}

	DAT.close();
}

void cc_calib::change_run_number(int which, int n)
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


string cc_calib::cc_nomenclature(int c)  ///< Translate 1-36 into 1-18 L/R 
{
	string y;
	int ch = c + 1; 
	if(ch%2)    // odd channels
	{
		y.append("L-");
		y += stringify((ch+1)/2);
	}
	else       // even channels
	{
		y.append("R-");
		y += stringify(ch/2);
	}

	return y;
}


void cc_calib::reset_pars()
{
	ccOpt->mean_min = 120;
	ccOpt->mean_max = 400;
	ccOpt->sigm_min = 30;
	ccOpt->sigm_max = 300;
	ccOpt->back_min = ccOpt->args["MIN_FIT_RANGE"].arg;
	ccOpt->back_max = ccOpt->args["MAX_FIT_RANGE"].arg;
}





