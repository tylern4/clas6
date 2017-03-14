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

int tpecal_calib::getADCs(int subtract)
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args + " BOS > ";
	int    NMAX   = (int) tpecalOpt->args["N"].arg;
	int    PEDS   = (int) tpecalOpt->args["PROCESS_PED"].arg;
	int    MIPS   = (int) tpecalOpt->args["PROCESS_MIP"].arg;
	int    BEAM   = (int) tpecalOpt->args["PROCESS_BEAM"].arg;
	double MIN_N  =       tpecalOpt->args["MIN_NEIGHBOR"].arg;
	
	int Nevents = 1;
	// bos reading variables
	// int firsttime;
	char command[100];

	// data histos
	TH1F TPECal_tdc[30];
	TH1F TPECal_adc[30];
	TH1F TPECal_adc_Single[30];
		
	TH1F RUNN     =  TH1F("runn",     "runn",      1, 0, 1);
	TH2F PARS     =  TH2F("pars",     "pars",     30, 0, 30, 5, 0, 5);
	TH1F MEANS    =  TH1F("means",    "means",    30, 0, 30);
	TH1F EMEANS   =  TH1F("emean",    "emean",    30, 0, 30);

	TTree *tpetree = new TTree("tpe",     "TPE ADC and TDC");
	int evn, nhits, id[31], adc[31], tdc[31];
	double SE[31];
	tpetree->Branch("evn",   &evn,    "evn/I");
	tpetree->Branch("nhits", &nhits,  "nhits/I");
	tpetree->Branch("id",    &id,     "id[nhits]/I");
	tpetree->Branch("adc",   &adc,    "adc[nhits]/I");
	tpetree->Branch("tdc",   &tdc,    "tdc[nhits]/I");


	// Init histograms
	for(int ss=0; ss<30;ss++)
	{
		if(MIPS) 
		{
			TPECal_adc[ss]         = TH1F(Form("Ch_%d",     ss+1), "", 600, 100, 9000);
			TPECal_adc_Single[ss]  = TH1F(Form("SCh_%d",    ss+1), "", 600, 100, 9000);
		}
		if(BEAM) 
		{
			TPECal_adc[ss]         = TH1F(Form("Ch_%d",     ss+1), "", 400, 0.2, 8);
			TPECal_adc_Single[ss]  = TH1F(Form("SCh_%d",    ss+1), "", 400, 0.2, 8);
		}
		if(PEDS) TPECal_adc[ss]  = TH1F(Form("Ch_%d",     ss+1), "", 400, 200,  600);
		TPECal_tdc[ss]           = TH1F(Form("tdc_Ch_%d", ss+1), "", 360,   1, 7200);
		TPECal_adc[ss].Fill(0);
		TPECal_adc_Single[ss].Fill(0);
		TPECal_tdc[ss].Fill(0);
		ped_old[ss] = 0;
		mip_old[ss] = 0;
	}
	
	clasTPE0_t *TPE0 = NULL;
	clasTPE1_t *TPE1 = NULL;
	clasHEAD_t *HEAD = NULL;

	// Files Loop
	for (unsigned int f=0; f<tpecalOpt->ifiles.size(); f++)
	{
		cout << hd_msg << " Opening file: " << tpecalOpt->ifiles[f] << endl;

		// It's a trick to execute the fortran command, wrapped in fparm_c
		sprintf(command, "OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", tpecalOpt->ifiles[f].c_str());
		initbos();
		if (!fparm_c(command)) cout << "\nError opening file   " << tpecalOpt->ifiles[f] << "\n\n\n" <<  endl;
		else
		{
			// Looking for first HEAD entry to set the runno
			while ( !(HEAD = (clasHEAD_t *) getBank(&bcs_, "HEAD")) && getBOS(&bcs_, 1, (char*) "E"));
			runno = HEAD->head[0].nrun;
			
			// gets number of events from DB. RUN CONTROL not working?
			// map_get_int("RUN_CONTROL.map", "events", "raw", 1, Nevent, runno, &firsttime);
			cout << hd_msg << " Run Number: " << runno << endl;

			if(subtract) read_ped_db();
			{
				cout << hd_msg << " Pedestal Values: " ;
				for(int i=0; i<30; i++)
				{
					if(i%6 == 0) cout << endl << "   > " ;
					cout << ped_old[i] << " " ;
				}
			}
			cout << endl;
			
			if(BEAM)
			{
				read_mip_db();
				cout << hd_msg << " MIP Values: " ;
				for(int i=0; i<30; i++)
				{
					if(i%6 == 0) cout << endl << "   > " ;
					cout << mip_old[i] << " " ;
				}
			}
			cout << endl << endl;

			// %% BOS Event loop
			while (getBOS(&bcs_,1, (char*) "E") && (NMAX == 0 || Nevents < NMAX))
			{
				HEAD = (clasHEAD_t *) getBank(&bcs_, "HEAD");
				TPE1 = (clasTPE1_t *) getGroup(&bcs_,"TPE1", 0);
				TPE0 = (clasTPE0_t *) getGroup(&bcs_,"TPE0", 0);
			
				if(!TPE1) continue;
				
				evn   = HEAD->head[0].nevent;
				nhits = TPE1->bank.nrow;
			
				// initializing tree variables to zero
				for(int i=0; i<31; i++)
				{
					id[i]   = 0;
					adc[i]  = 0;
					tdc[i]  = 0;
				}
								
				// ADC
				if(TPE1)
				{
					for(int I=0; I<TPE1->bank.nrow; I++)
					{		
						id[I]  = TPE1->tpe1[I].id-1;
						adc[id[I]] = TPE1->tpe1[I].adc - (int) ped_old[id[I]];			
						
						// TDC
						if(subtract && TPE0)
						{
							for(int J=0;J<TPE0->bank.nrow;++J)
							{
								if(id[I] == TPE0->tpe0[J].id-1)
								{
									tdc[id[I]] = TPE0->tpe0[J].tdc;
									if(id[I] < 30 && adc[I] > 100 )
										TPECal_tdc[id[I]].Fill(tdc[id[I]]);
										
									continue;
								}
							}
						}
						
					}
						
						
					for(int I=0; I<TPE1->bank.nrow; I++)
					{					
						id[I]  = TPE1->tpe1[I].id-1;
						int neigh = 0;
						// list of neighbours for this channel
						vector<int> neighb = neighbors(id[I]);
						
						
						// checking if a neighbour hit has signal 
						for(unsigned int j=0; j<neighb.size(); j++)
						{
							if(adc[neighb[j]] > MIN_N) neigh++;
						}
						
						if(MIPS)
						{
							if(id[I] < 30) 
								TPECal_adc[id[I]].Fill(adc[id[I]]);
							if(neigh == 0 && id[I] < 30 )
								TPECal_adc_Single[id[I]].Fill(adc[id[I]]);
						}
						
						// For the beam, need to convert to energy
						// Summing the energies of the neighbors
						if(BEAM)
						{
							for(int z=0; z<31; z++) 
								SE[z] = 0;
							if(adc[id[I]] < 8200)	
								SE[id[I]] += adc[id[I]]*(400.0/mip_old[id[I]])/1000.0;
							//if(id[I] < 30) cout << id[I] << " " << adc[I] << " " << " " << SE[id[I]] << " neighbors: ";
							for(unsigned int j=0; j<neighb.size(); j++)
							{
								//cout << neighb[j] << " " ;
								if(adc[id[I]] < 8200 && adc[neighb[j]] < 0.5*adc[id[I]])
									SE[id[I]] += adc[neighb[j]]*(400.0/mip_old[neighb[j]])/1000.0;
							}	
							//if(id[I] < 30) cout << "  Total energy: " << SE[id[I]] << endl;
							if(id[I] < 30) TPECal_adc[id[I]].Fill(SE[id[I]]);
							if(adc[id[I]] < 8200)
								if(id[I] < 30) TPECal_adc_Single[id[I]].Fill(adc[id[I]]*(400.0/mip_old[id[I]])/1000.0);
						}
						
					}
				}	
				
				if(Nevents++%10000 == 0) cout << hd_msg << Nevents - 1 << " events analyzed." << endl;;
				dropAllBanks(&bcs_, (char*) "E");
				cleanBanks(&bcs_);
				//tpetree->Fill();
			}
			// %% End of BOS Event loop

		}
		sprintf(command,"CLOSE BOSOUTPUT UNIT=1");
		fparm_c(command);
	}
	cout << hd_msg << " Total Number of Events Analyzed: " << Nevents << endl;

 
	if(subtract) rfile = "tpe_mip_"  + stringify(runno) + ".root";
	if (BEAM)    rfile = "tpe_beam_" + stringify(runno) + ".root";
	else         rfile = "tpe_ped_"  + stringify(runno) + ".root";
	cout << hd_msg << " Writing histos to ROOT file: " << rfile << endl;

	TFile output(rfile.c_str(), "RECREATE","");
	for(int ss=0; ss<30;ss++)
	{
		TPECal_tdc[ss].Write();
		TPECal_adc[ss].Write();
		TPECal_adc_Single[ss].Write();
	}
	
	RUNN.SetBinContent(1, runno);
	RUNN.Write();
	PARS.Write();
	MEANS.Write();
	EMEANS.Write();
//	tpetree->Write();
	output.Write();
	output.Close();

	return 1;
}

