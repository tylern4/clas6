// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "usage.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <iostream>
#include <fstream>
#include <cstdio>
#include <set>
#include <cstdlib>

cc_opts::cc_opts()
{
	args["LOG_MSG"].args = " >>> Cerenkov";
	args["LOG_MSG"].help = "Log Messages Header.";
	args["LOG_MSG"].name = "Log Messages Header";
	args["LOG_MSG"].type = 1;
	args["LOG_MSG"].ctgr = "verbosity";

	args["USE_QT"].arg  = 1;
	args["USE_QT"].help = "Use/Don't Use the QT GUI.";
	args["USE_QT"].name = "QT Gui";
	args["USE_QT"].type = 0;
	args["USE_QT"].ctgr = "control";

	args["ROOT_FILE"].args = "";
	args["ROOT_FILE"].help = "Opens ROOT file.";
	args["ROOT_FILE"].name = "Opens ROOT file.";
	args["ROOT_FILE"].type = 1;
	args["ROOT_FILE"].ctgr = "control";

	args["RUN_INDEX"].args = "calib.RunIndex";
	args["RUN_INDEX"].help = "Run Index Table Name.";
	args["RUN_INDEX"].name = "Run Index Table Name";
	args["RUN_INDEX"].type = 1;
	args["RUN_INDEX"].ctgr = "database";

	args["RUN_INDEX_MIN_RUN"].arg  = -1;
	args["RUN_INDEX_MIN_RUN"].help = "Run Index Min Run Number";
	args["RUN_INDEX_MIN_RUN"].name = "Run Index Min Run Number";
	args["RUN_INDEX_MIN_RUN"].type = 0;
	args["RUN_INDEX_MIN_RUN"].ctgr = "database";
	
	args["DBRUN"].arg  = -1;
	args["DBRUN"].help = "Run Index reference Run Number";
	args["DBRUN"].name = "Run Index reference Run Number";
	args["DBRUN"].type = 0;
	args["DBRUN"].ctgr = "database";

	args["RUNNO"].arg  = -1;
	args["RUNNO"].help = "Run Number: in case the root file is the sum of many, the run infos need to be corrected.";
	args["RUNNO"].name = "Run Number";
	args["RUNNO"].type = 0;
	args["RUNNO"].ctgr = "database";

	args["RUN_INDEX_MAX_RUN"].arg  = 1000000;
	args["RUN_INDEX_MAX_RUN"].help = "Run Index Max Run Number";
	args["RUN_INDEX_MAX_RUN"].name = "Run Index Max Run Number";
	args["RUN_INDEX_MAX_RUN"].type = 0;
	args["RUN_INDEX_MAX_RUN"].ctgr = "database";

	args["PROCESS_SPE"].arg  = 0;
	args["PROCESS_SPE"].help = "Process SPE Bos File";
	args["PROCESS_SPE"].name = "Process SPE Bos File";
	args["PROCESS_SPE"].type = 0;
	args["PROCESS_SPE"].ctgr = "control";

	args["PROCESS_PED"].arg  = 0;
	args["PROCESS_PED"].help = "Process PED Bos File";
	args["PROCESS_PED"].name = "Process PED Bos File";
	args["PROCESS_PED"].type = 0;
	args["PROCESS_PED"].ctgr = "control";

	args["PROCESS_ROOT"].arg  = 0;
	args["PROCESS_ROOT"].help = "Process ROOT File";
	args["PROCESS_ROOT"].name = "Process ROOT File";
	args["PROCESS_ROOT"].type = 0;
	args["PROCESS_ROOT"].ctgr = "control";

	args["DATA"].arg  = 0;
	args["DATA"].help = "Histogram limits enlarged for data";
	args["DATA"].name = "Histogram limits enlarged for data";
	args["DATA"].type = 0;
	args["DATA"].ctgr = "control";

	args["TDC"].arg  = 0;
	args["TDC"].help = "Fills histogram only if TDC is non zero.";
	args["TDC"].name = "Fills histogram only if TDC is non zero.";
	args["TDC"].type = 0;
	args["TDC"].ctgr = "control";


	args["MIN_FIT_RANGE"].arg  = 50.0;
	args["MIN_FIT_RANGE"].help = "Lower value of fit range";
	args["MIN_FIT_RANGE"].name = "Lower value of fit range";
	args["MIN_FIT_RANGE"].type = 0;
	args["MIN_FIT_RANGE"].ctgr = "control";
	
	args["MAX_FIT_RANGE"].arg  = 600.0;
	args["MAX_FIT_RANGE"].help = "Lower value of fit range";
	args["MAX_FIT_RANGE"].name = "Lower value of fit range";
	args["MAX_FIT_RANGE"].type = 0;
	args["MAX_FIT_RANGE"].ctgr = "control";
	
}

cc_opts::~cc_opts(){}

int cc_opts::Set(int argc, char **argv)
{
	string arg;
	string com;
	string opt;
	cout << endl;
	string comp;

	map<string, opts>::iterator itm;

	set<string> category;

	// Filling Categories
	for(itm = args.begin(); itm != args.end(); itm++)
		if(category.find(itm->second.ctgr) == category.end()) category.insert(itm->second.ctgr);



	// -help-all
	for(int i=1; i<argc; i++)
	{
		arg = argv[i];
		com = "-help-all";
		if(arg == com)
		{
			cout <<  "    Usage: -Option=<option>" << endl << endl;
			cout <<  "    Options:" <<  endl << endl ;

			for(itm = args.begin(); itm != args.end(); itm++)
				cout <<  "   > Option " <<  itm->first << ": " << itm->second.help << endl;

			cout << endl << endl;
			exit(0);
		}
	}


	// -help
	set<string> :: iterator itcat;
	for(int i=1; i<argc; i++)
	{
		arg = argv[i];
		com = "-help";
		if(arg == com)
		{
			cout <<  endl << endl;
			cout <<  "    Help Options:" <<  endl << endl ;
			cout <<  "   >  -help-all:  all available options. " <<  endl << endl;
			for(itcat = category.begin(); itcat != category.end(); itcat++)
			{

				cout <<  "   >  -help-" << *itcat << "     ";
				cout.width(15);
				cout << *itcat << " options." << endl;
			}
			cout << endl << endl;
			exit(0);
		}
	}


	// -help-option
	for(int i=1; i<argc; i++)
	{
		arg = argv[i];
		for(itcat = category.begin(); itcat != category.end(); itcat++)
		{
			com = "-help-" + *itcat;
			if(arg == com)
			{
				cout << endl << endl <<  "   ## " << *itcat << " ## " << endl << endl;
				cout << "    Usage: -Option=<option>" << endl << endl;
				for(itm = args.begin(); itm != args.end(); itm++)
					if(itm->second.ctgr == *itcat) cout <<  "   > " <<  itm->first << ": " << itm->second.help << endl;
				cout << endl << endl;
				exit(0);
			}
		}
	}

	// If Maps option are specified, overriding default
	for(int i=1; i<argc; i++)
	{
		arg = argv[i];
		map<string, opts>::iterator itm; 
		for(itm = args.begin(); itm != args.end(); itm++)
		{
			com = "-" + itm->first + "=";
			comp.assign(arg, 0, arg.find("=") + 1);
			if(comp == com)
			{
				opt.assign(arg, com.size(), arg.size()-com.size());
				itm->second.args = opt;
				itm->second.arg  = atof(opt.c_str());
				cout <<  " >>> Options: " << itm->second.name << " set to: " ;
				if(itm->second.type) cout << itm->second.args;
				else cout << itm->second.arg  ;
				cout << endl;
			}
		}
	}

	// If argument is a file, adding it to the list of files
	for(int i=1; i<argc; i++)
	{
		ifstream isFile(argv[i], ios::in | ios::binary);
		if(isFile) ifiles.push_back(argv[i]);
	}

	cout << endl;

	for(unsigned int i=0; i<ifiles.size(); i++)
		cout << " Input file found: " << ifiles[i] << endl; 

	cout << endl;

	return 1;
}




