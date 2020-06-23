
#include "trk_mon_lite.h"


// Re-declare the bos common
BOSbank bcs_;
BOSbank wcs_;

// Globals
int DONE=0;
int RUNNUMBER = 0;
int USER_SPECIFIED_RUNNUMBER = -1;
int SHOW_TICKER=1; // print info to screen every second to user we're working
int PROTON_CUT=1;
int SINGLE_SECTOR=0;
float VERTEX_CUT_LOW=-100.0;
float VERTEX_CUT_HIGH=50.0;

// Function prototypes
void UpdateTicker(int Nevents);
void Usage(void);
void ParseCommandLineArguments(int narg, char* argv[]);
void ctrlCHandle(int);
extern "C" int main(int narg,char *argv[]);

//-------------
// main
//-------------
int main(int narg,char *argv[])
{
	int Nevents = 0;

	// Catch HUP and INT signals and exit gracefully
	signal(SIGINT,ctrlCHandle);
	signal(SIGHUP,ctrlCHandle);

	// Initialize local angle cuts
	//	for(int reg=1;reg<=3;reg++){
	//	LOCANGLE_CUT_HIGH[reg]=60.0;
	//	LOCANGLE_CUT_LOW[reg]=-60.0;
	//}
	LOCANGLE_CUT_HIGH[3]=8.0;
	LOCANGLE_CUT_HIGH[2]=-2.0;
	LOCANGLE_CUT_HIGH[1]=-14.0;
	LOCANGLE_CUT_LOW[3]=-8.0;
	LOCANGLE_CUT_LOW[2]=-13.0;
	LOCANGLE_CUT_LOW[1]=-27.0;


	// Parse command line arguments
	ParseCommandLineArguments(narg,argv);

	// Initialize BOS
	int maxnames=200;
	bnames_(&maxnames);
	initbos();

	// Loop over files
	for(int i = 1;i < narg; ++i) {
		char *argptr = argv[i];
		if (*argptr == '-')continue;

		int valid_input_stream = initFile(argptr);
		if(!valid_input_stream){
			cerr<<__FILE__<<":"<<__LINE__<<" Error opening file \""<<argptr<<"\""<<endl;
			continue;
		}
		cout<<"File \""<<argptr<<"\" opened."<<endl;		

		// Loop over events in file
		while(!DONE){
			int ret = getData(&bcs_,"E");
			if(ret == 0)break;
			if(ret!=DISIO_DATA)continue;

			// Update number of events
			Nevents++;
			if(SHOW_TICKER)UpdateTicker(Nevents);

			// Initialize on first event
			if(Nevents == 1) {
				// Get HEAD bank and extract run number
				clasHEAD_t *HEAD = (clasHEAD_t*)getBank(&bcs_, "HEAD");
				if(HEAD)RUNNUMBER=HEAD->head[0].nrun;
				if(USER_SPECIFIED_RUNNUMBER>=0)RUNNUMBER=USER_SPECIFIED_RUNNUMBER;
				make_RUNC_bank(RUNNUMBER); // This MUST be here for the tracking geometry to be set up period.
				//dc_begin_run(RUNNUMBER);
				dc_xvst_init_(&RUNNUMBER);
				hist_book(RUNNUMBER);
			}
			
			// Fill histograms/Ntuples
			if(hist_fill())DONE=1;
			
			// Clear event from BOS memory
			dropAllBanks(&bcs_,"E"); /*drop everything in the E list*/
			cleanBanks(&bcs_);
			
		} // --- end event loop ---
		
		// Close BOS file
		fparm_c("CLOSE BOSINPUT");
		
		if(DONE)break;
	} // --- end file loop---

	cout<<endl<<endl;
	//	if(TOTAL_NTUPLE_ROWS>0)cout<<TOTAL_NTUPLE_ROWS<<" Ntuple rows written."<<endl;
	int sum=0;
	for(int sup=1; sup<=6; sup++) sum += TOTAL_NTUPLE_ROWS[sup];
	if( sum ) cout<<sum<<" Ntuple rows written."<<endl;
	//	if(TOTAL_NTUPLE_ROWS>0)cout<<TOTAL_NTUPLE_ROWS<<" Ntuple rows written."<<endl;
	cout<<Nevents<<" events processed."<<endl;

	// Close hbook file
	hist_end();

	// Update quality database (if specified)
	if(UPDATE_QUALITY_DATABASE)UpdateQualityDB();

	return 0;
}

//----------------------
// UpdateTicker
//----------------------
void UpdateTicker(int Nevents)
{
	static time_t last_time=0;
	time_t now = time(NULL);
	if(now-last_time>0){
		static int last_nevents=0;
		cout<<"  "<<Nevents<<" events ("<<Nevents-last_nevents<<"Hz)     \r";
		cout.flush();
		last_nevents = Nevents;
		last_time=now;
	}
}

//----------------------
// ParseCommandLineArguments
//----------------------
void ParseCommandLineArguments(int narg, char* argv[])
{
	int reg;

	// Loop over command line arguments
	for(int i=1; i<narg; i++) {
		char *argptr = argv[i];
		if (*argptr == '-') {
			argptr++;
			switch (*argptr) {
			case 'h':
			  Usage();
			  break;
			case 'o':
			  strcpy(OUTPUTFILE, ++argptr);
			  break;
			case 'r':
			case 'R':
			  USER_SPECIFIED_RUNNUMBER =atoi(++argptr);
			  break;
			case 'C':
			  tm_set_ntuple_cut(argptr);
			  break;
			case 'S':
			  SINGLE_SECTOR=atoi(++argptr);
			  if(SINGLE_SECTOR<0 || SINGLE_SECTOR>6) SINGLE_SECTOR=0;
			  cout<<"Fill ntuples only for sector:"<<SINGLE_SECTOR<<endl;
			  break;
			case 'F':
			  AUTOFIND_LOCAL_ANGLE_CUTS=1;
			  // no break for this case(we want LOCAL_ANGLE_HISTOS_ONLY set too)
			case 'L':
			  LOCAL_ANGLE_HISTOS_ONLY=1;
			  break;
			case 'U':
			  UPDATE_QUALITY_DATABASE=1;
			  break;
			case 'P':
			  PROTON_CUT=0;
			  break;
			case 'V':
			  if(argptr[1]=='l' || argptr[1]=='u') {
			    float f=atof(&argptr[2]);
			    if(argptr[1]=='l'){
			      VERTEX_CUT_LOW=f;
			      cout << "Vertex cut: low "<<VERTEX_CUT_LOW<<endl;
			    }
			    else {
			      VERTEX_CUT_HIGH=f;
			      cout << "Vertex cut: high "<<VERTEX_CUT_HIGH<<endl;
			    }
			  }
			  break;
			case 'A':
			  reg=atoi(&argptr[1]);
			  if(reg>=1 && reg<=3){
			    if(argptr[2]!='l' && argptr[2]!='u'){
			      cerr<<endl<<endl<<"\n\nArgument must be -A#l# or -A#u# ."<<endl;
			      exit(-1);
			    }
			    float f = atof(&argptr[3]);
			    if(argptr[2]=='l') {
			      LOCANGLE_CUT_LOW[reg]=f;
			      cout<<"Local Angle cut:  reg["<<reg<<"] L.A.  Low: "<<f<<":"<<endl;
			    }else{
			      LOCANGLE_CUT_HIGH[reg]=f;
			      cout<<"Local Angle cut:  reg["<<reg<<"] L.A. High: "<<f<<":"<<endl;
			    }
			  }
			  break;
			default:
			  cerr<<"Unrecognized argument: [-"<<argptr<<"]"<<endl;
			  Usage();
			  break;
			}
		}
	}
	
	// Set PROTON mass cut by default unless flag was cleared
	if(PROTON_CUT){
		extern int mass_cut_var;
		mass_cut_var=1;
		tm_set_mass_upper(0.938+0.200);
		tm_set_mass_lower(0.938-0.200);
	}
}

//----------------------
// Usage
//----------------------
void Usage(void)
{
	fprintf(stderr,"Usage: trk_mon_lite [options] file1 [file2] ....\n\n");
	fprintf(stderr,"  Options:\n");
	fprintf(stderr,"\t-o<outfile>\t Output file name\n");
	fprintf(stderr,"\t-r[#]\t\t Use run number # for initializations etc..\n");
	fprintf(stderr,"\t-C\t\t Set cut on Ntuple events (e.g. -Cbeta.lt.0.9).\n");
	fprintf(stderr,"\t-F\t\t Auto-find local angle cuts.\n");
	fprintf(stderr,"\t-L\t\t Produce local angle histos only (no ntuples!).\n");
	fprintf(stderr,"\t-U\t\t Update CLAS DC quality database.\n");
	fprintf(stderr,"\t-S[#]\t\t Only data for single sector (e.g. -S1).\n");
	fprintf(stderr,"\t-Vl[#]\t\t lower cut on vertex (e.g. -Vl-10.0).\n");
	fprintf(stderr,"\t-Vu[#]\t\t upper cut on vertex (e.g. -Vu10.0).\n");
	fprintf(stderr,"\t-P\t\t Do NOT cut on protons (default is to cut on protons).\n");
	fprintf(stderr,"\t-A\t\t Set cut on local angle(in degrees) by region. e.g.\n");
	fprintf(stderr,"\t  \t\t -A2u12.4  sets the upper limit cut on region 2 to 12.4\n");
	fprintf(stderr,"\t  \t\t the first character after A is the region, the second \n");
	fprintf(stderr,"\t  \t\t is either\"u\" for upper or \"l\" for lower followed\n");
	fprintf(stderr,"\t  \t\t by the value.\n");
	fprintf(stderr,"\t-h\t\t Print this message.\n");
	fprintf(stderr,"\n");

	fprintf(stderr,"DC Versions:\n");
	fprintf(stderr,"\t dclib: %d.%d\n",DC_DCLIB_VERSION_MAJOR,DC_DCLIB_VERSION_MINOR);
	fprintf(stderr,"\t dc.h : %d.%d(dclib)    %d.%d(trk_mon_lite)\n"
		,DC_DCH_VERSION_MAJOR,DC_DCH_VERSION_MINOR
		,DC_DCH_VERSION_MAJOR_h,DC_DCH_VERSION_MINOR_h);

	exit(0);
}

//----------------------
// ctrlCHandle
//----------------------
void ctrlCHandle(int x)   /* exit the program gracefully and save ntuple if interrupted with ctrl-c. */
{
	signal(SIGINT,ctrlCHandle);
	signal(SIGHUP,ctrlCHandle);
	cerr<<"--- Interrupt caught. Ending ---"<<endl;
	DONE = 1;
}

