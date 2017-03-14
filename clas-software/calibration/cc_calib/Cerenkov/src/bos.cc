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

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cmath>

// pmt numbers are in order,
// so in each sector one must do
// N_local = 1 + remainder (N/36)
extern "C"
{
#include <ntypes.h>
#include <bostypes.h>
#include <mysql.h>
#include <map_manager.h>
}

extern BOSbank bcs_;    // Bos Common Structure

int cc_calib::getADCs(int subtract)
{
	string hd_msg = ccOpt->args["LOG_MSG"].args + " BOS > ";
	int Nevents = 1;

	// bos reading variables
	// int firsttime;
	char command[100];

	TH1F CC_adc[6][36];
	TH1F RUNN     =  TH1F("runn",     "runn",     1, 0, 1);
	TH3F PARS     =  TH3F("pars",     "pars",     6, 0, 6, 36, 0, 36, 5, 0, 5);
	TH2F MEANS    =  TH2F("means",    "means",    6, 0, 6, 36, 0, 36);
	TH2F EMEANS   =  TH2F("emean",    "emean",    6, 0, 6, 36, 0, 36);
	TH2F CHI2S    =  TH2F("chi2s",    "chi2s",    6, 0, 6, 36, 0, 36);
	TH2F POISMUS  =  TH2F("poissMus", "poissMus", 6, 0, 6, 36, 0, 36);
	TH2F POISPS   =  TH2F("poissPs",  "poissPs",  6, 0, 6, 36, 0, 36);

	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
		{
			if(ccOpt->args["DATA"].arg == 0) CC_adc[s][ss] = TH1F(Form("Sect_%d__segm_%d", s+1, ss+1), "", 450,   -50,  850);
			if(ccOpt->args["DATA"].arg == 1) CC_adc[s][ss] = TH1F(Form("Sect_%d__segm_%d", s+1, ss+1), "", 510,  -100,  5000);
			ped_old[s][ss] = 0;
		}

	clasCC0_t  *CC0  = NULL;
	clasHEAD_t *HEAD = NULL;

	// Files Loop
	for (unsigned int f=0; f<ccOpt->ifiles.size(); f++)
	{
		cout << hd_msg << " Opening file: " << ccOpt->ifiles[f] << endl;

		// It's a trick to execute the fortran command, wrapped in fparm_c
		sprintf(command, "OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", ccOpt->ifiles[f].c_str());
		initbos();
		if (!fparm_c(command)) cout << "\nError opening file   " << ccOpt->ifiles[f] << "\n\n\n" <<  endl;
		else
		{
			// Looking for first HEAD entry to set the runno
			while ( !(HEAD = (clasHEAD_t *) getBank(&bcs_, "HEAD")) && getBOS(&bcs_, 1, (char*) "E"));
			runno = HEAD->head[0].nrun;
			
			// gets number of events from DB. RUN CONTROL not working?
			// map_get_int("RUN_CONTROL.map", "events", "raw", 1, Nevent, runno, &firsttime);
			cout << hd_msg << " Run Number: " << runno << endl;

			if(subtract) read_ped_db();
			int TDC = (int) ccOpt->args["TDC"].arg;

			// %% BOS Event loop
			while (getBOS(&bcs_,1, (char*) "E"))
			{
				for(int sect=1; sect<7; sect++)
				{
					CC0 = (clasCC0_t *) getGroup(&bcs_,"CC  ", sect);
					if(CC0)
						for(int I=0;I<CC0->bank.nrow;++I)
						{
							if(TDC == 1 && CC0->cc0[I].tdc > 0) CC_adc[sect-1][CC0->cc0[I].id-1].Fill(CC0->cc0[I].adc-ped_old[sect-1][CC0->cc0[I].id-1]);
							if(TDC == 0)                        CC_adc[sect-1][CC0->cc0[I].id-1].Fill(CC0->cc0[I].adc-ped_old[sect-1][CC0->cc0[I].id-1]);
						}
				}
				if(Nevents++%1000000 == 0) cout << hd_msg << Nevents - 1 << " events analyzed." << endl;;
				dropAllBanks(&bcs_, (char*) "E");
				cleanBanks(&bcs_);
			}
			// %% End of BOS Event loop

		}
		sprintf(command,"CLOSE BOSOUTPUT UNIT=1");
		fparm_c(command);
	}
	cout << hd_msg << " Total Number of Events Analyzed: " << Nevents << endl;

 
	if(subtract) rfile = "cc_adc_spe_" + stringify(runno) + ".root";
	else         rfile = "cc_adc_ped_" + stringify(runno) + ".root";
	cout << hd_msg << " Writing histos to ROOT file: " << rfile << endl;

	TFile output(rfile.c_str(), "RECREATE","");
	for(int s=0; s<6;s++)
		for(int ss=0; ss<36;ss++)
			CC_adc[s][ss].Write();

	RUNN.SetBinContent(1, runno);
	RUNN.Write();
	PARS.Write();
	MEANS.Write();
	EMEANS.Write();
	CHI2S.Write();
	POISMUS.Write();
	POISPS.Write();
	output.Write();
	output.Close();

	return 1;
}

