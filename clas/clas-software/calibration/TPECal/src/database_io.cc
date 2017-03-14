// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "tpecal_calib.h"


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


void tpecal_calib::init_db()
{
	setenv("CLAS_CALDB_RUNINDEX", tpecalOpt->args["RUN_INDEX"].args.c_str(), 1);
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	cout << hd_msg << " Initializing Run Index: " << getenv("CLAS_CALDB_RUNINDEX") << endl;
	// have to do this first to make it all work
	float HV[216];
	int firsttime;
	map_get_float("CC_CALIB", "HV", "value", 216, HV, 30400, &firsttime);
}


void tpecal_calib::read_hv_db()
{
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	float HV[30];
	int firsttime;
	if(runno>0)
	{
		cout << hd_msg << "Connecting to database " << tpecalOpt->args["RUN_INDEX"].args 
				<< " to get HV for Run Number " << runno << "..." << endl;
		map_get_float("TPECal_CALIB.map", "HV", "value", 30, HV, runno, &firsttime);
		for(int p=0; p<30; p++)
		{
			TPECal_HV_OLD[p] = (double) HV[p];
		}	
		hv_b = true;
	}
	else
	{
		cout << hd_msg << " Run Number is not set." << endl;
	}
}


void tpecal_calib::write_hv_db()
{
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	float HV[30];
	
	for(int p=0; p<30; p++)
	{
		HV[p]   = (float) TPECal_HV_NEW[p]; 
		cout << p << " " << HV[p] << endl;
	}	
	map_put_float("TPE.map", "HV", "value", 30, HV, min_runno);
}




void tpecal_calib::read_ped_db()
{
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	// db reading variables
	float Pedestal[30];
	int firsttime;
	if(runno>0)
	{
		cout << hd_msg << "Connecting to database " << tpecalOpt->args["RUN_INDEX"].args 
		               << " to get pedestals for Run Number " << runno << "..." << endl;
		map_get_float("TPE", "pedestal", "mean", 30, Pedestal, runno, &firsttime);
		
		for(int p=0; p<30; p++)
		{
			ped_old[p] = Pedestal[p]; // From total to local index
		}	
	}
	else
	{
		cout << hd_msg << " Run Number is not set." << endl;
	}
}

void tpecal_calib::write_ped_db()
{
	float PED[30], SIG[30];
	for(int i=0; i<30; i++)
	{
		PED[i] = ped_value[i];
		SIG[i] = ped_sigma[i];
	}

	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	cout << hd_msg << "Connecting to database " << tpecalOpt->args["RUN_INDEX"].args 
                 << " to put pedestals for Run Number " << min_runno << "..." << endl;

	map_put_float("TPE", "pedestal", "mean" , 30, PED, min_runno);
	map_put_float("TPE", "pedestal", "sigma", 30, SIG, min_runno);
}

void tpecal_calib::read_mip_db()
{
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	int dbrun        = (int) tpecalOpt->args["DBRUN"].arg;
	if(dbrun == -1)
		dbrun = runno;
	
	// db reading variables
	float mip[30];
	int firsttime;
	if(dbrun>0)
	{
		cout << hd_msg << "Connecting to database " << tpecalOpt->args["RUN_INDEX"].args 
		               << " to get MIP for Run Number " << dbrun << "..." << endl;
		map_get_float("TPE", "mip", "amplitude", 30, mip, dbrun, &firsttime);
		
		for(int p=0; p<30; p++)
		{
			mip_old[p] = mip[p]; // From total to local index
		}
	}
	else
	{
		cout << hd_msg << " Run Number is not set." << endl;
	}
}

void tpecal_calib::write_mip_db()
{
	string hd_msg    = tpecalOpt->args["LOG_MSG"].args + " Database: >> " ;
	float SPE[30];
	
	for(int p=0; p<30; p++)
	{
		SPE[p] = mip_value[p];
	}
	map_put_float("TPECal_CALIB.map", "mip", "amplitude", 30, SPE, min_runno);
}


