// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "cc_calib.h"


// %%%%%%%%%%%%
// CLAS headers
// %%%%%%%%%%%%
extern "C"
{
#include <mysql.h>
#include <map_manager.h>
#include <calib_manager.h>
#include <map_manager.h>
}

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cstdlib>
#include <cmath>


void cc_calib::init_db()
{
	setenv("CLAS_CALDB_RUNINDEX", ccOpt->args["RUN_INDEX"].args.c_str(), 1);
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	cout << hd_msg << " Initializing Run Index: " << getenv("CLAS_CALDB_RUNINDEX") << endl;
	// have to do this first to make it all work
	float HV[216];
	int firsttime;
	map_get_float("CC_CALIB.map", "HV", "value", 216, HV, 30400, &firsttime);
}


void cc_calib::change_run_index(string new_run_index)
{
	// ccOpt->args["RUN_INDEX"].args = new_run_index;
	// init_db();
	cout <<  endl << endl; 
	cout <<  " Unfortunately one can't change the Run Index on the fly." << endl;
	cout <<  " Please use the \"RUN_INDEX\" option to set the Run Index at the start of the program." << endl << endl;
}

void cc_calib::read_hv_db()
{
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	float HV[216];
	int firsttime;
	if(runno>0)
	{
		cout << hd_msg << "Connecting to database " << ccOpt->args["RUN_INDEX"].args << " to get HV for Run Number " << runno << "..." << endl;
		map_get_float("CC_CALIB.map", "HV", "value", 216, HV, runno, &firsttime);
		int SECT, CHANN;
		for(int p=0; p<216; p++)
		{
			SECT   = p/36;
			CHANN  = (int) fmod((double) p, 36);
			CC_HV_OLD[SECT][CHANN] = (double) HV[p];
		}	
		hv_b = true;
	}
	else
	{
		cout << hd_msg << " Run Number is not set." << endl;
	}
}


void cc_calib::write_hv_db()
{
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	float HV[216];
	
	int SECT, CHANN;
	for(int p=0; p<216; p++)
	{
		SECT    = p/36;
		CHANN   = (int) fmod((double) p, 36);
		HV[p]   = (float) CC_HV_NEW[SECT][CHANN]; 
		cout << p << " " << HV[p] << endl;
	}	
	map_put_float("CC_CALIB.map", "HV", "value", 216, HV, min_runno);
}




void cc_calib::read_ped_db()
{
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	// db reading variables
	float Pedestal[216];
	int firsttime;
	if(runno>0)
	{
		cout << hd_msg << "Connecting to database " << ccOpt->args["RUN_INDEX"].args << " to get pedestals for Run Number " << runno << "..." << endl;
		map_get_float("CC_CALIB.map", "pedestals", "mean", 216, Pedestal, runno, &firsttime);
		
		int SECT, CHANN;
		for(int p=0; p<216; p++)
		{
			SECT   = p/36;
			CHANN  = (int) fmod((double) p, 36);
			ped_old[SECT][CHANN] = Pedestal[p]; // From total to local index
		}	
	}
	else
	{
		cout << hd_msg << " Run Number is not set." << endl;
	}
}

void cc_calib::write_ped_db()
{
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	int PED[216];
	float SIG[216];

	int SECT, CHANN;
	for(int p=0; p<216; p++)
	{
		SECT   = p/36;
		CHANN  = (int) fmod((double) p, 36);
		PED[p] = (const int) ped_value[SECT][CHANN];
		SIG[p] = ped_sigma[SECT][CHANN];
	}
	map_put_int(  "CC_CALIB.map", "pedestals", "mean" , 216, PED, min_runno);
	map_put_float("CC_CALIB.map", "pedestals", "sigma", 216, SIG, min_runno);
}

void cc_calib::read_spe_db()
{
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	int dbrun        = ccOpt->args["DBRUN"].arg;
	if(dbrun == -1)
		dbrun = runno;
	
	// db reading variables
	float spe[216];
	int firsttime;
	if(dbrun>0)
	{
		cout << hd_msg << "Connecting to database " << ccOpt->args["RUN_INDEX"].args << " to get SPE for Run Number " << dbrun << "..." << endl;
		map_get_float("CC_CALIB.map", "photoE", "amplitude", 216, spe, dbrun, &firsttime);
		
		int SECT, CHANN;
		for(int p=0; p<216; p++)
		{
			SECT   = p/36;
			CHANN  = (int) fmod((double) p, 36);
			spe_old[SECT][CHANN] = spe[p]; // From total to local index
		}
	}
	else
	{
		cout << hd_msg << " Run Number is not set." << endl;
	}
}

void cc_calib::write_spe_db()
{
	string hd_msg    = ccOpt->args["LOG_MSG"].args + " Database: >> " ;
	float SPE[216];
	
	int SECT, CHANN;
	for(int p=0; p<216; p++)
	{
		SECT = p/36;
		CHANN = (int) fmod((double) p, 36);
		SPE[p] = spe_value[SECT][CHANN];
	}
	map_put_float("CC_CALIB.map", "photoE", "amplitude", 216, SPE, min_runno);
}


