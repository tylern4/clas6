/*
 * g12Anal.cc   
 * 
 */
using namespace std;
#include <iostream>
extern "C" {

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>


#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <sc.h>
#include <PartUtil.h>
#include <dataIO.h>
#include <itape.h>
#include <vertex.h>
#include <trk_run_control.h>
#include <trk.h>
#include <makebanks.h>
void dc_set_def_();
void trk_set_def_();
}
#include <plib.h>
#include <Vec.h>
#include <lorentz.h>
#include <pputil.h>
#include <mwKfit.h>
#include <clasEvent.h>
#include <g12Anal.h>

/*bitwise drop flags*/
#define DROP_RAW BIT(0)
#define DROP_DC0 BIT(1)
#define DROP_DC1 BIT(2)
#define DROP_HBLA BIT(3)
#define DROP_TBLA BIT(4)
#define DROP_HBTB BIT(5)
#define DROP_SC BIT(6)
#define DROP_EC BIT(7)
#define DROP_HBID BIT(8)
#define DROP_CL01 BIT(9)
#define DROP_SEB BIT(10)
#define DROP_TBID BIT(11)
#define DROP_HDPL BIT(12)
#define DROP_LAC BIT(13)
#define DROP_CC BIT(14)
#define DROP_ST BIT(15)
#define DROP_DHCL BIT(16)
#define DROP_TAGR BIT(17)
#define DROP_TDPL BIT(18)

#define CC_BANKS "CCRCCC01"
#define SEB_BANKS "HEVTEVNTDCPBSCPBCCPBUNUSEVHBTRKSSTPBTGPBLCPB"
#define SC_BANKS "SC1 SCR SCRC"
#define EC_BANKS "EC01ECHBECPIECPCECPBEC  EC1 "
#define ST_BANKS "ST1 STR "
#define REGION1_BANKS "RGLK"

#define SCALER_EVENT 10
#define DATA_EVENT 1

#define BUFSIZE 200000

int matlab = 0;

int resetScaler = 1;
double totalTime = 0.0;
#define TIME_CNTS 100000

double eBeamLo = 4.4, eBeamHi = 6.0;

/* ----------- Function prototypes ---------------- */
string id2name(Particle_t type);
int ProcessGeneralHeader(clasEvent &evt, int debug);
void printGeneralHeaderLabels(int *nlab);
int
		SelectEvent(int nmode, int *modeN, clasEvent &evt, double beamRange0,
				double beamRange1, double massRange0, double massRange1,
				int reverseFlag);

int Q(Particle_t pid);
int GetDispatcherCommand();
int ProcessEvent(clasEvent &, unsigned int, int, int, int debug);
int ProcessScaler(clasEvent &, int);
int GetDat(FILE *finput, char *, int);
void ctrlCHandle(int);
void PrintUsage(char *processName);
int StartRun(int);
int EndRun(int);
int ProcessData(itape_header_t *buffer);
int dispatcherReconnect(const char*host, int pipelinePrefill);
int printLabels(Event_t);
int printGeneralLabels(char *);
void pHeader(clasEvent &evt);
int processPhotons(clasEvent &evt, int debug, int partbank);
int printPhotonLabels(int nlab);
void printTrackLabels(int *nlab, char *id1, char *id2);
int cut(float x, float low, float hi);
int getPi0x(int partbank, fourVec &pi0, int *igam1, int *igam2);
int makePartPi0(int pb, int addbank);
int Statistics(int, clasEvent &w);
int triggerStatistics(int, clasEvent &w);
int ProcessChargedPart(clasEvent &, int debug, int);
int ProcessNeutrals(clasEvent &, int debug, int);
void ProcessNPipPip(Event_t, clasEvent &, int debug);
void ProcessPipPi0(Event_t, clasEvent &, int debug);
int ProcessPipPim(Event_t, clasEvent &, int debug);
int ProcessPPipPi0(Event_t, clasEvent &, int debug);
void ProcessPPipPimGamGam(Event_t, clasEvent &, int debug);
int ProcessPKm(Event_t mode, clasEvent &evt, int debug);
int ProcessK(Event_t mode,clasEvent &evt,int debug);
int ConfigGeom(int);
void convertNeutrons2gammas(int partbank0, int, int);
extern "C" {
void bnames_(int *);
int SetVerbose(int);
int initDisplay(char *, int);
int getData(BOSbank *, char *);
void ProcessVert(clasEvent &, int debug);
int ProcessUnknown(Event_t, clasEvent &, int debug);
int ProcessProtonProton(Event_t, clasEvent &, int debug);
void ProcessAll(Event_t, clasEvent &, int debug);
void ProcessETIME(clasEvent &, int debug);
void processTrack(clasParticle &cp);
void remakeNeutrals(clasEvent &evt, int, double, int, int);
void remakeNeutralFromVertex(clasEvent &evt, int partbank0,
		double neutronBetaCut, int silentMode, int debug);
void ProcessEp(Event_t, clasEvent &, int debug);
void ProcessEm(Event_t, clasEvent &, int debug);

void ProcessP(Event_t, clasEvent &, int debug);
void ProcessPip(Event_t, clasEvent &, int debug);
void ProcessPim(Event_t, clasEvent &, int debug);
void ProcessPipPip(Event_t, clasEvent &, int debug);
void ProcessPPip(Event_t, clasEvent &, int debug);
void ProcessPPim(Event_t, clasEvent &, int debug);
void ProcessPPipPim(Event_t, clasEvent &, int debug);
void ProcessKpKp(Event_t, clasEvent &, int debug);
int ProcessKpKpPim(Event_t, clasEvent &, int debug);
void ProcessPPipGam(Event_t, clasEvent &, int debug);
void ProcessPPipPimGam(Event_t, clasEvent &, int debug);
void ProcessPipPimGam(Event_t, clasEvent &, int debug);
void ProcessPipPipPimGam(Event_t, clasEvent &evt, int debug);
int ProcessKpPipPim(Event_t, clasEvent &evt, int debug);
	
int ProcessPKpKmPipPim(Event_t, clasEvent &evt, int debug);

// electron/positron stuff

void ProcessEpEm(Event_t, clasEvent &evt, int debug);

int processCut(int cutMode, clasEvent &evt);
int installFaultHandlers();
static void signalINT(int isig);
static void signalSEGV(int isig);
fourVec echb2partMVRT(echb_t *echb, double mass, threeVec &v, int silentMode);
int DropList(int DropFlag);
/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

}

static int requestedRawData = 0;
int CurrentRun = 0;
int CurrentEvent = 0;
int partbank = 1;
int makeSEBpart = 0;
int SEBpart = 6;
int Simulation = 0;

extern particleDataTable PDGtable;

void printDropFlags() {

	cerr << "RAW:\t" << hex << DROP_RAW << dec << endl;
	cerr << "DC0:\t" << hex << DROP_DC0 << dec << endl;
	cerr << "DC1:\t" << hex << DROP_DC1 << dec << endl;
	cerr << "HBLA:\t" << hex << DROP_HBLA << dec << endl;
	cerr << "TBLA:\t" << hex << DROP_TBLA << dec << endl;
	cerr << "HBTB:\t" << hex << DROP_HBTB << dec << endl;
	cerr << "SC:\t" << hex << DROP_SC << dec << endl;
	cerr << "EC:\t" << hex << DROP_EC << dec << endl;
	cerr << "HBID:\t" << hex << DROP_HBID << dec << endl;
	cerr << "CL01:\t" << hex << DROP_CL01 << dec << endl;
	cerr << "SEB:\t" << hex << DROP_SEB << dec << endl;
	cerr << "TBID:\t" << hex << DROP_TBID << dec << endl;
	cerr << "HDPL:\t" << hex << DROP_HDPL << dec << endl;
	cerr << "LAC:\t" << hex << DROP_LAC << dec << endl;
	cerr << "CC:\t" << hex << DROP_CC << dec << endl;
	cerr << "ST:\t" << hex << DROP_ST << dec << endl;
	cerr << "DHCL:\t" << hex << DROP_DHCL << dec << endl;
	cerr << "TAGR:\t" << hex << DROP_TAGR << dec << endl;
	cerr << "TDPL:\t" << hex << DROP_TDPL << dec << endl;

}

void PrintUsage(char *processName) {
	cerr << processName << endl;
	cerr << "\t-L[m]\tprint labels for mode m (default: all modes)" << endl;
	cerr << "\t-oFileName\tWrite bos output to FileName" << endl;
	cerr << "\t-v\tVerbose mode" << endl;
	cerr << "\t-m\tcorrect momentum" << endl;
	cerr << "\t-t#\ttrigger mask" << endl;
	cerr << "\t-M#\tprocess # events" << endl;
	cerr << "\t-S#\tFirst skip # events" << endl;
	cerr << "\t-x#\tChoose mode # for output (may have more than 1)" << endl;
	cerr << "\t-s\tSilent mode: no printout" << endl;
	cerr << "\t-p#\tUse PART bank # for analysis (default = 0)" << endl;
	cerr
			<< "\t-P#\tUse PART bank # for constructing new PART banks (default = 0)"
			<< endl;
	cerr << "\t-A[#]\tAdd PART bank # (default = 4)" << endl;
	cerr << "\t-R\tRegenerate the TBID and associated banks" << endl;
	cerr << "\t-T\tRegenerate the appropriate PART bank" << endl;
	cerr << "\t-E[#]\tMake part bank # from SEB (default = 6)" << endl;
	cerr << "\t-C\tConvert neutrons to gammas" << endl;
	cerr << "\t-V\tMake vertices" << endl;
	cerr << "\t-Z\tchange Z position for neutrals from MVRT" << endl;
	cerr << "\t-n#\tSet neutron beta cut to # (default = 0.9)" << endl;
	cerr << "\t-d\tDebug mode" << endl;
	cerr << "\t-e\tInclude eloss package" << endl;
	cerr << "\t-rb#,#\tCut on beam energy" << endl;
	cerr << "\t-rm#,#\tCut on MM^2 (normalized to 0 by mode)" << endl;
	cerr << "\t-G\tUse GPID" << endl;
	cerr << "\t-X\tcut mode" << endl;
	cerr << "\t[-cm#]\t\tSet mini-torus current to #" << endl;
	cerr << "\t[-ct#]\t\tSet torus current to #" << endl;
	cerr << "\t-h\tprint the above" << endl;
}

int SelectEvent(int nmode, int *modeN, clasEvent &evt, double beamRange0,
		double beamRange1, double massRange0, double massRange1, int reverseFlag) {
	int ret = !nmode;
	double ebeam = evt.beam().get4P().t();
	double mmsq;

	if (!reverseFlag) {

		for (int i = 0; i < nmode; ++i) {
			if (isEvent((Event_t) modeN[i], evt)) {
				mmsq = MMsq( (Event_t) modeN[i], evt);
				ret = (mmsq < massRange1) && (mmsq > massRange0);
			}
		}

		ret = ret && ((ebeam > beamRange0) && (ebeam < beamRange1));
	} else {
		ret = 1;

		for (int i = 0; i < nmode; ++i) {
			ret = ret && !isEvent((Event_t) modeN[i], evt);
		}

	}

	return (ret);
}

int ProcessScaler(clasEvent &event, int silentMode) {

	static int n = 0;
	static unsigned int prevLIVE = 0;
	int ret = 0;
	clasTRGS_t *TRGS = event.TRGS(0);
	if (TRGS) {
		ret = 1;
		if (!silentMode) {
			cout << "CLK " << event.run() << " " << event.event() << " " << n++
					* 10 << " " << TRGS->trgs[0].microsec << " "
					<< TRGS->trgs[0].clock_ug << " " << TRGS->trgs[0].clock_g1
					<< " " << TRGS->trgs[0].clock_g2 << " ";
			cout << endl;
		}
		if (resetScaler) {
			prevLIVE = TRGS->trgs[0].clock_g2;
		} else {
			totalTime += (double) (TRGS->trgs[0].clock_g2 - prevLIVE)/TIME_CNTS;
		}

	}
	return (ret);
}

int ProcessGeneralHeader(clasEvent &evt, int debug) {

	pHeader(evt);
	cout << ~(evt.beam().get4P() + evt.target().get4P()) << " ";
	cout << evt.beam().get4P().t() << " ";
	cout << evt.x() << " " << evt.y() << " " << evt.z() << " ";
	cout << evt.N(-1) << " " << evt.N(0) << " " << evt.N(1) << " ";
	cout << evt.vtime() << " " << evt.stVtime() << " " << evt.vtime() - evt.stVtime() << " ";
	return (1);
}

void printGeneralHeaderLabels(int *nlab) {
	char *intro = "";

	if (matlab)
		intro = "%\t";
	cout << intro << ++(*nlab) << "\tRun {run}" << endl;
	cout << intro << ++(*nlab) << "\tEvent {evt}" << endl;
	cout << intro << ++(*nlab) << "\ttrigger {trig}" << endl;
	cout << intro << ++(*nlab) << "\tmodes {modes}" << endl;
	cout << intro << ++(*nlab) << "\tsqrt(s) {wcm}" << endl;
	cout << intro << ++(*nlab) << "\tE of beam {ebeam}" << endl;
	cout << intro << ++(*nlab) << "\tx {x}" << endl;
	cout << intro << ++(*nlab) << "\ty {y}" << endl;
	cout << intro << ++(*nlab) << "\tz {z}" << endl;
	cout << intro << ++(*nlab) << "\t# of q=-1 {nm}" << endl;
	cout << intro << ++(*nlab) << "\t# of q=-0 {n0}" << endl;
	cout << intro << ++(*nlab) << "\t# of q=-1 {np}" << endl;
	cout << intro << ++(*nlab) << "\tvtime {vtime}" << endl;
	cout << intro << ++(*nlab) << "\tstVtime {stvtime}" << endl;
	cout << intro << ++(*nlab) << "\tvtime - stVtime {delvt}" << endl;
}

int main(int argc, char **argv) {
	int max = 0;
	int verbose = 0;
	int ret = 1;
	int cutMode = 0;
	int nacc = 0;
	int Nevents = 0;
	int Nrecords = 0;
	int Nread = 0;
	int Nwrite = 0;
	int nScaler = 0;

	int i;
	char *argptr;
	char *word;
	int Dispatch = 0;
	unsigned int triggerMask = 0;

	char *TList= NULL;

	int addBank = -1;
	int partbank0 = 1;

	int regen = 0;
	int makepart = 0;

	int modeN[10];
	int nmode = 0;

	int silentMode = 0;

	int convertNeutrons = 0;
	int convertGammas = 0;
	int makeVert = 0;
	int changeNeutralZ = 0;

	int NoFault = 0;

	int debug = 0;

	// timing
	int t0;
	int t1 = 0;

	int nskip = 0;

	int useGPID = 0;

	int useSEB = 0;

	double neutronBetaCut = 0.9;

	double massRange0 = -1000.0, massRange1 = 1000.0, beamRange0 = -1000.0,
			beamRange1 = 1000.0;

	// magnets
	int torus = 0;
	int mini = 0;
	int torusCurrent;
	int minitorusCurrent;

	// ouput

	clasOutput coutput;
	int status = 0;

	// Dispatcher


	// itape stuff

	// bos stuff
	int OutputUnitNo = 9, MaxBanks = 1000;

	Event_t mode;

	int reverseFlag = 0;

	int nTagged = 0;
	int nBins = 0;

	int eLoss = 0;
	int correctMomentum = 0;

	double timeCut = 0.0;
	double trackTimeCut = 0.0;


	int DropFlag = 0x0; /*don't drop anything*/

	for (i = 0; i < argc; ++i)
		cerr << argv[i] << " ";
	cerr << endl;

	for (i=1; i<argc; i++) {
		argptr = argv[i];

	
		if (*argptr == '-') {
			argptr++;
			switch (*argptr) {
			case 'L':
				mode = (Event_t) atoi(++argptr);
			if (!printGeneralLabels(argptr))
					printLabels(mode);
				break;

			case 'G':
				useGPID = 1;
				break;

			case 'Z':
				changeNeutralZ = 1;
				break;

			case 'o':
				if (*(++argptr)) {
					unlink(argptr);
					status = coutput.open(argptr, OutputUnitNo);
					cerr << "Output file: " << argptr << endl;
				}
				break;
			case 'f':
				// do not install fault handlers
				NoFault = 1;
				break;

			case 's':
				silentMode = 1;
				break;

			case 'W':
				TList = ++argptr;
				break;

			case 'Y':
				useSEB = 1;
				break;

			case 'E':
				makeSEBpart = 1;
				if (strlen(++argptr)) {
					SEBpart = atoi(argptr);
				}
				cerr << "Make SEB bank # " << SEBpart << endl;
				break;

			case 'e':
				eLoss = 1;
				break;

			case 'S':
				nskip = atoi(++argptr);
				break;
			case 'd':
				debug = 1;
				break;

			case 'r':
				++argptr;
				switch (*argptr) {
				case 'b':
					argptr++;
					word = strtok(argptr, ",");
					beamRange0 = atof(word);
					word = strtok(NULL," ");
					beamRange1 = atof(word);
					cerr << "Beam range: " << beamRange0 << " --> "
							<< beamRange1 << endl;
					break;
				case 'm':
					argptr++;
					word = strtok(argptr, ",");
					massRange0 = atof(word);
					word = strtok(NULL," ");
					massRange1 = atof(word);
					cerr << "Missing mass^2 range: " << massRange0 << " --> "
							<< massRange1 << endl;

					break;
				}

			case 'D':
				DropFlag = strtoul(++argptr, NULL, 0);
				cerr << "Drop Flag: " << hex << DropFlag << dec << endl;
				break;
			case 'v':
				verbose++;
				SetVerbose(1);
				break;
			case 'p':
				partbank = atoi(++argptr);
				break;
			case 'P':
				partbank0 = atoi(++argptr);
				break;
			case 'M':
				max = atoi(++argptr);
				break;

			case 'n':
				neutronBetaCut = atof(++argptr);
				cerr << "neutron beta cut = " << neutronBetaCut << endl;
				break;

			case 't':
				triggerMask = strtoul(++argptr, NULL, 0);
				break;

			case 'm':
				correctMomentum = 1;
				break;

			case 'h':
				PrintUsage(argv[0]);

				if (verbose) {
					printModes();
					if (verbose > 1)
						printDropFlags();
				}
				exit(0);
				break;

			case 'x':
				modeN[nmode++] = atoi(++argptr);
				break;

			case 'A':
				addBank = strlen(++argptr) ? atoi(argptr) : 4;
				cerr << "Making PART bank " << addBank << endl;
				break;
			case 'R':
				regen =1;
				break;
			case 'T':
				makepart =1;
				break;
			case 'C':
				convertNeutrons = 1;
				if (*(++argptr) == 'x') {
					convertGammas = 1;

					cerr << "Convert gammas" << endl;
				}
				break;

			case 'c':
				switch (*(++argptr)) {
				case 't':
					torus = 1;
					torusCurrent= atoi(++argptr);
					cerr << "torus current set to " << torusCurrent << " Amps"
							<< endl;
					break;
				case 'm':
					mini = 1;
					minitorusCurrent= atoi(++argptr);
					cerr << "minitorus current set to " << minitorusCurrent
							<< " Amps" << endl;
					break;
				default:
					PrintUsage(argv[0]);
					break;
				}
				break;

			case 'X':
				cutMode = atoi(++argptr);
				break;

			case 'V':
				makeVert = 1;
				break;

				break;

			default:
				cerr << "Unrecognized argument: " << argptr << endl;
				;
				break;

			}

		} else if (*argptr == '+') {
			argptr++;
			switch (*argptr) {
			
			case 't':
			  timeCut = (double) atof(++argptr);
			  break;
			case 'T':
			  trackTimeCut = (double) atof(++argptr);
			  break;
			case 'x':
				reverseFlag = 1;
				modeN[nmode++] = atoi(++argptr);
				break;
			}
		}
	}

	Simulation = !partbank;
	PDGtable.initialize();

	if (!NoFault)
		installFaultHandlers();

	// Initialize BOS
	bnames_(&MaxBanks);
	initbos();
	configure_banks(stderr,0);

	cerr << "Drop Flag: " << hex << DropFlag << dec << endl;

	// we need to do this another way
	dc_set_def_();
	trk_set_def_();
	if (mini)
		trktcl_.ipar_minitorus_current = minitorusCurrent;
	if (torus)
		trktcl_.ipar_torus_current = torusCurrent;

	for (i = 1; i < argc; ++i) {
		argptr = argv[i];
		if (*argptr != '-' && *argptr != '+') {
			clasEvent evt(argptr, &bcs_, partbank0, 0);
			evt.verbose(verbose);
			cerr << "initialize:\t" << argptr << endl;

			if (useGPID)
				evt.useGPID();
			if (useSEB)
				evt.useSEB();

			Dispatch = isDispatcher(argptr);
			if (evt.status()) {
				ret = 1;

				while ((max ? nacc < max : 1) && ret) {
					int reLoad = 0;
					clasHEAD_t *HEAD;
					ret = evt.read(partbank0); // still one event?
					if (ret == DISIO_DATA) {
						Nread++;
						if ((HEAD = (clasHEAD_t *)getBank(&bcs_, "HEAD"))) {
							int runno=HEAD->head[0].nrun;
							CurrentRun = HEAD->head[0].nrun;
							CurrentEvent = HEAD->head[0].nevent;
							//	      ConfigEvent(runno,regen); 
							/* Initialize the TOF geometry.  This is needed regardless of 
							 whether you remake the SC reconstruction banks or not.  However,
							 for the regeneration case, this is done in ConfigEvent. */
							if (!regen)
								ConfigGeom(runno);
							StartRun(runno);

							if (Nevents == 1)
								cerr << "Processing run period "
										<< runPeriod(evt.runPeriod()) << endl;

							// process


							/*re-make TBID banks*/
							if (regen) {
								dropAllBanks(&bcs_, BID_BANKS);
								bankList(&bcs_, "E+", BID_BANKS);
								make_BID_banks(partbank0);
								reLoad = 1;
							}
							if (makepart) {
								dropAllBanks(&bcs_, "PART");
								bankList(&bcs_, "E+", "PART");
								make_PART_group(partbank0);
								reLoad = 1;
							}
							if (makeSEBpart) {
								clasEVNT_t *EVNT = (clasEVNT_t *)getBank(&bcs_,
										"EVNT");
								if (EVNT) {
									makePART_from_EVNT(EVNT, SEBpart);
									reLoad = 1;
								}
							}
							if (useGPID)
								bankList(&bcs_, "E+", "TDPL");
							if (makeVert) {
								bankList(&bcs_, "E+", "VERTMVRT");
								make_vert();
								make_mvrt();
								reLoad = 1;
							}
							bankList(&bcs_, "E+","TDPLTBLA");
							evt.buildSwim();
							
							if (convertNeutrons) {
								convertNeutrons2gammas(partbank0,
										convertGammas, silentMode);
								reLoad = 1;
							}
							if (makeVert && !changeNeutralZ) {
								remakeNeutrals(evt, partbank0, neutronBetaCut,
										silentMode, debug);
								reLoad = 1;
							} else if (changeNeutralZ) {
								remakeNeutralFromVertex(evt, partbank0,
										neutronBetaCut, silentMode, debug);
								reLoad = 1;
							}
							makePartPi0(partbank0, addBank);

							// Reload the new group
							if (reLoad)
								evt.load(partbank);

							triggerStatistics(0, evt);

							// timing
							if (Nevents == 1) {
								t0 = evt.time();
							} else if (t0 > evt.time()) {
								t0 = evt.time();
							}
							if (t1 < evt.time()) {
								t1 = evt.time();
							}

							if (evt.type() == DATA_TYPE || evt.type()
									== MONTE_CARLO_TYPE) {

							  if ( (timeCut > 0.0 ? fabs(evt.stVtime() - evt.vtime()) < timeCut : 1) &&  SelectEvent(nmode, modeN, evt, beamRange0, beamRange1, massRange0, massRange1,reverseFlag)) {
									nacc++;

									Statistics(0, evt);

									if (triggerMask ? evt.trig() & triggerMask
											: 1)
										triggerStatistics(1, evt);

									if (eLoss)
										evt.eLoss();

									if (correctMomentum)
										evt.momentumCorrection();

									

									ProcessEvent(evt, triggerMask, silentMode,
											cutMode, debug);

									if (evt.type() == SCALER_EVENT) {
										nScaler++;
									}

									nTagged += evt.nTagged(50, 150, eBeamLo,
											eBeamHi);
									nBins++;

								}
								if (status && (timeCut > 0.0 ? fabs(evt.stVtime() - evt.vtime()) < timeCut : 1)&& SelectEvent(nmode, modeN, evt,
										beamRange0, beamRange1, massRange0,
										massRange1, reverseFlag)) {
									if (triggerMask ? evt.trig() & triggerMask
											: 1) {

										if (DropFlag)
											DropList(DropFlag);
										if (!nskip) {
											if (TList) {
												bankList(&bcs_, "T=", TList);
												coutput.write(&bcs_, TList);
											} else {
												coutput.write(&bcs_);
											}
											Nwrite++;
										} else {
											nskip--;
										}
									}
								}
							}
							if (evt.type() == 1 || evt.type() == -2
									|| evt.type() == -4) {
								Nevents++;
							}
							if (!(Nevents % 1000))
								cerr << Nevents << "\t" << Nrecords << "\t"
										<< Nwrite << "\r" << flush;
							Nrecords++;
							evt.clean();

						}
					} else if (ret == DISIO_COMMAND) {

						cerr << "Message from Giant Head: " << getBuffer()
								<< endl;;

					}

				}

				cerr << "\nFlux:\t" << (float)nTagged/(float)nBins
						<< " x 10^7/sec" "" << endl;
				cerr << "\nTotal (scalar) time:\t" << totalTime << endl;
				cerr << "Total (wallclock) time:\t" << t1 - t0 << " " << t0
						<< " " << t1 << endl;

				cerr << "\nTotal number of itape records read:\t" << Nread
						<< endl;
				cerr << "\nTotal number of itape records:\t" << Nrecords
						<< endl;
				cerr << "\nTotal number of itape events:\t" << Nevents << endl;
				cerr << "\tTotal number of records written:\t" << Nwrite
						<< endl;
				cerr << "\tTotal number of scaler records:\t" << nScaler
						<< endl;
				Statistics(1, evt);
				triggerStatistics(2, evt);

			} else {
				cerr << "Unable to open " << argptr << endl;
			}

		}
	}
	if (Dispatch)
		disIO_command("FINISHED");
	cerr << "\nTotal number of itape events:\t" << Nevents << endl;
	if (status) {
		coutput.write(&bcs_, "0");
		coutput.close();
		cerr << coutput.name() << " closed" << endl;
	}
}

void remakeNeutralFromVertex(clasEvent &evt, int partbank0,
		double neutronBetaCut, int silentMode, int debug) {
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank0);
	clasMVRT_t *MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	clasECHB_t *ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
	if (TBID && MVRT && PART && ECHB) {
		threeVec p1;
		fourVec p2;
		double mass= NEUTRON_MASS;
		threeVec v(MVRT->mvrt[0].vert.x, MVRT->mvrt[0].vert.y,
				MVRT->mvrt[0].vert.z);
		threeVec r;
		double p;
		for (int k = 0; k < PART->bank.nrow; ++k) {
			part_t *part = &PART->part[k];
			p1.set(part->p.space.x, part->p.space.y, part->p.space.z);
			p2.set(0.0, p1);
			switch ((Particle_t) part->pid) {
			case Gamma:
				mass = 0.0;
			case Neutron:

				// Get the corresponding echb bank
				tbid_t *tbid = &TBID->tbid[part->trkid - 1];
				if ((tbid->ec_id > 0) && tbid->ec_stat) {
					echb_t *echb = &ECHB->echb[tbid->ec_id - 1];
					r.set(echb->x_hit - v.x(), echb->y_hit - v.y(), echb->z_hit
							- v.z());
					if ((Particle_t)part->pid == Neutron) {
						part->pid = Neutron;
						mass = NEUTRON_MASS;
						p = p1.r();
						p2 = echb2partMVRT(echb, 0.0, v, silentMode);
						p1.set(p2.x(), p2.y(), p2.z());
						p1 *= p/p1.r();
						p2.set(sqrt(mass * mass + p1.r() * p1.r()), p1);
					} else {
						part->pid = Gamma;
						mass = 0.0;
						p2 = echb2partMVRT(echb, mass, v, silentMode);
					}
					part->p.space.x = p2.x();
					part->p.space.y = p2.y();
					part->p.space.z = p2.z();
					part->p.t = p2.t();
					part->vert.x = v.x();
					part->vert.y = v.y();
					part->vert.z = v.z();
				}
				break;
			
			}
		}
	}
}
void remakeNeutrals(clasEvent &evt, int partbank0, double neutronBetaCut,
		int silentMode, int debug) {
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank0);
	clasMVRT_t *MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	clasECHB_t *ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
	// clasTGPB_t *TGPB = (clasTGPB_t *) getBank(&bcs_,"TGPB");
#define LIGHTSPEED 30 // cm/nanosec
	fourVec p;
	part_t *part;
	echb_t *echb;
	tbid_t *tbid;
	double mass = 0.0;
	//if (TBID && MVRT && PART && ECHB && TGPB) {
	if (TBID && MVRT && PART && ECHB) {
		threeVec p1;
		fourVec p2;
		threeVec v(MVRT->mvrt[0].vert.x, MVRT->mvrt[0].vert.y,
				MVRT->mvrt[0].vert.z);
		threeVec r;
		double beta, p;
		for (int k = 0; k < PART->bank.nrow; ++k) {
			part = &PART->part[k];
			p1.set(part->p.space.x, part->p.space.y, part->p.space.z);
			p2.set(0.0, p1);
			switch ((Particle_t) part->pid) {
			case Gamma:
			case Neutron:
				// Get the corresponding echb bank
				tbid = &TBID->tbid[part->trkid - 1];
				if ((tbid->ec_id > 0) && tbid->ec_stat) {
					echb = &ECHB->echb[tbid->ec_id - 1];
					r.set(echb->x_hit - v.x(), echb->y_hit - v.y(), echb->z_hit
							- v.z());
					//beta = r.r()/((echb->t_hit - TGPB->tgpb[0].time) * LIGHTSPEED);
					beta = r.r()/((echb->t_hit - tbid->vtime) * LIGHTSPEED);
					if (!silentMode) {
						cout << "BETA " << " ";
						ProcessGeneralHeader(evt, debug);
						cout << beta << endl;
					}
					if (beta < neutronBetaCut) {
						part->pid = Neutron;
						mass = NEUTRON_MASS;
						p = NEUTRON_MASS*tbid->ec_beta*beta2gamma(tbid->ec_beta);
						p2 = echb2partMVRT(echb, 0.0, v, silentMode);
						p1.set(p2.x(), p2.y(), p2.z());
						p1 *= p/p1.r();
						p2.set(sqrt(mass * mass + p1.r() * p1.r()), p1);
					} else {
						part->pid = Gamma;
						mass = 0.0;
						p2 = echb2partMVRT(echb, mass, v, silentMode);
					}
					part->p.space.x = p2.x();
					part->p.space.y = p2.y();
					part->p.space.z = p2.z();
					part->p.t = p2.t();
					part->vert.x = v.x();
					part->vert.y = v.y();
					part->vert.z = v.z();
				}
				break;
			default:
				break;
			}
		}
	}
}
int ProcessNeutrals(clasEvent &evt, int debug, int partbank0) {
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank0);
	clasMVRT_t *MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	clasECHB_t *ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
	clasTGPB_t *TGPB = (clasTGPB_t *) getBank(&bcs_, "TGPB");

	fourVec p;
	part_t *part;
	echb_t *echb;
	tbid_t *tbid;
	if (!evt.gpid()) {

		if (TBID && PART && ECHB && TGPB) {
			threeVec p1;
			fourVec p2;
			threeVec v;
			threeVec r;
			double beta;
			if (MVRT)
				v.set(MVRT->mvrt[0].vert.x, MVRT->mvrt[0].vert.y,
						MVRT->mvrt[0].vert.z);
			else
				v.set(0.0, 0.0, 0.0);
			for (int k = 0; k < PART->bank.nrow; ++k) {
				part = &PART->part[k];
				p1.set(part->p.space.x, part->p.space.y, part->p.space.z);
				p2.set(0.0, p1);
				switch ((Particle_t) part->pid) {
				case Gamma:
				case Neutron:
					// Get the corresponding echb bank
					tbid = &TBID->tbid[part->trkid - 1];
					if ((tbid->ec_id > 0) && tbid->ec_stat) {
						echb = &ECHB->echb[tbid->ec_id - 1];
						r.set(echb->x_hit - v.x(), echb->y_hit - v.y(),
								echb->z_hit - v.z());
						beta = r.r()/((echb->t_hit - TGPB->tgpb[0].time)
								* LIGHTSPEED);
						cout << "NEUT ";
						ProcessGeneralHeader(evt, debug);
						cout << part->pid << " ";
						cout << beta << " ";
						cout << p1.r() << " ";
						cout << v.x() << " " << v.y() << " " << v.z() << " ";
						cout << endl;
					}
					break;
				default:
					break;
				}
			}
		}
	}
	return (1);
}

void convertNeutrons2gammas(int partbank0, int convertGammas, int silentMode) {
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank0);
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	clasECHB_t *ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
	threeVec v(0.0, 0.0, 0.0);
	fourVec p;
	part_t *part;
	echb_t *echb;
	tbid_t *tbid;
	if (PART && TBID && ECHB) {
		for (int k = 0; k < PART->bank.nrow; ++k) {
			part = &PART->part[k];
			switch ((Particle_t) part->pid) {
			case Neutron:
				tbid = &TBID->tbid[part->trkid - 1];
				if ((tbid->ec_id > 0) && tbid->ec_stat) {
					echb = &ECHB->echb[tbid->ec_id - 1];
					part->pid = (Particle_t) Gamma;
					p = echb2partMVRT(echb, NEUTRON_MASS, v, silentMode);
					part->p.t = p.t();
					part->p.space.x = p.x();
					part->p.space.y = p.y();
					part->p.space.z = p.z();
				}
				break;
			case Gamma:
				//	if (convertGammas)
				//part->pid = (Particle_t) Unknown;
				break;
			default:
				break;
			}
		}
	}
}
int makePartPi0(int partbank, int addbank) {
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	int igamma1, igamma2;
	if (addbank >= 0 && PART) {
		fourVec pi0;
		int addpi0 = getPi0x(partbank, pi0, &igamma1, &igamma2);
		int npart = addpi0 ? PART->bank.nrow - 1 : PART->bank.nrow;
		clasPART_t *newPART = (clasPART_t *)makeBank(&bcs_, "PART", addbank,
				sizeof(part_t)/sizeof(int), npart);
		for (int k = 0, j = 0; k < PART->bank.nrow; ++k) {
			if (addpi0 && (k == igamma1)) {
				newPART->part[j] = PART->part[k];
				newPART->part[j].p.t = pi0.t();
				newPART->part[j].p.space.x = pi0.x();
				newPART->part[j].p.space.y = pi0.y();
				newPART->part[j].p.space.z = pi0.z();
				newPART->part[j].pid = Pi0;
				newPART->part[j].q = 0.0;
				j++;
			} else if (addpi0 && (k == igamma2)) {
				;
			} else {
				newPART->part[j++] = PART->part[k];
			}
		}

	}
	return (1);
}

int processPhotons(clasEvent &evt, int partbank) {
	int ngamma = 0, ngam = 0;
	int nneutron = 0;
	fourVec gamma[3];
	float m[] = { -1000.0, -1000.0, -1000.0 };
	float E[] = { -1000.0, -1000.0, -1000.0 };
	float phi[] = { -1000.0, -1000.0, -1000.0 };
	float cosTheta[] = { -1000.0, -1000.0, -1000.0 };
	float Egg[] = { -1000.0, -1000.0, -1000.0 };
	float phigg[] = { -1000.0, -1000.0, -1000.0 };
	float cosThetagg[] = { -1000.0, -1000.0, -1000.0 };
	int sec[] = { -1000, -1000, -1000 };
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	clasECHB_t *ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
	tbid_t *tbid;
	echb_t *echb;

	if (evt.gpid()) {
		;
	} else {

		if (PART) {
			part_t *part;
			for (int i = 0; i < PART->bank.nrow; ++i) {
				part = &PART->part[i];
				switch ((Particle_t) part->pid) {
				case Gamma:
					ngam++;
					if (ngamma < 3) {
						gamma[ngamma].set(part->p.t, threeVec(part->p.space.x,
								part->p.space.y, part->p.space.z));
						E[ngamma] = gamma[ngamma].t();
						phi[ngamma] = gamma[ngamma].phi()/M_PI;
						cosTheta[ngamma] = gamma[ngamma].cosTheta();

						if (TBID && ECHB) {
							if (Simulation) {
								sec[ngamma] = 0;
							} else {
								tbid = &TBID->tbid[part->trkid - 1];
								if ((tbid->ec_id > 0) && tbid->ec_stat) {
									echb = &ECHB->echb[tbid->ec_id - 1];
									sec[ngamma] = echb->sect/100;
								}
							}
						}
						ngamma++;
					}

					break;
				case Neutron:
					nneutron++;
					break;
				default:
					break;
				}
			}
		}
	}

	for (int i = 0, k = 0; i < ngamma -1; ++i) {
		for (int j = i + 1; j < ngamma; ++j) {
			m[k] = ~(gamma[i] + gamma[j]);
			Egg[k] = (gamma[i] + gamma[j]).t();
			cosThetagg[k] = (gamma[i] + gamma[j]).cosTheta();
			phigg[k] = (gamma[i] + gamma[j]).phi()/M_PI;
		}
	}
	cout << " " << ngam << " " << nneutron << " ";
	for (int i = 0; i < 3; ++i)
		cout << E[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << m[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << phi[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << cosTheta[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << sec[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << Egg[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << cosThetagg[i] << " ";
	for (int i = 0; i < 3; ++i)
		cout << phigg[i] << " ";

	return (1);
}

int getPi0x(int partbank, fourVec &pi0, int *igam1, int *igam2) {
#define MPI0 .135
#define DELTA .075
	int ipi, jpi;
	int npi0 = 0;
	int ngamma = 0;
	fourVec gamma[3];
	int kpart[3];
	float m[] = { -1000.0, -1000.0, -1000.0 };
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	if (PART) {
		part_t *part;
		for (int i = 0; i < PART->bank.nrow; ++i) {
			part = &PART->part[i];
			switch ((Particle_t) part->pid) {
			case Gamma:
				if (ngamma < 3) {
					kpart[ngamma] = i;
					gamma[ngamma].set(part->p.t, threeVec(part->p.space.x,
							part->p.space.y, part->p.space.z));
					ngamma++;
				}

				break;
			default:
				break;
			}
		}
		for (int i = 0, k = 0; i < ngamma -1; ++i) {
			for (int j = i + 1; j < ngamma; ++j) {
				m[k] = ~(gamma[i] + gamma[j]);
				if (cut(m[k], MPI0 - DELTA, MPI0 + DELTA)) {
					npi0++;
					ipi = i;
					jpi = j;
				}
				k++;
			}
		}
	}
	if (npi0) {
		pi0 = gamma[ipi] + gamma[jpi];
		*igam1 = kpart[ipi];
		*igam2 = kpart[jpi];

	}
	return (npi0);

}


int ProcessK(Event_t mode, clasEvent &evt, double trackTimeCut,int debug) {
  int nKm = evt.N(KMinus);
  int nKp = evt.N(KPlus);
  int ret = 0;
  for (int i = 0; i < nKm; ++i) {
	clasParticle Km = evt.cp(KMinus, i + 1);
	ret += trackTimeCut > 0.0 ? abs(Km.stVtime() - evt.vtime()) < trackTimeCut : 1;
  }
  for (int i = 0; i < nKp; ++i) {
	clasParticle Kp = evt.cp(KPlus, i + 1);
	ret += trackTimeCut > 0.0 ? abs(Kp.stVtime() - evt.vtime()) < trackTimeCut : 1;
  }
  return(ret > 0);

}

void ProcessEp(Event_t mode, clasEvent &evt, int debug) {
	clasParticle Ep = evt.cp(Positron, 1);
	fourVec ep = Ep.p();
	fourVec beam=evt.beam().get4P(), target=evt.target().get4P();
	cout << "M38 ";
	ProcessGeneralHeader(evt, debug);
	cout << ep.t() << " ";
	cout << ep.V().r() << " " << ep.V().cosTheta() << " " << ep.V().phi()/M_PI << " ";
	cout << Ep.ecEnergy() << " " << Ep.ecTime() << " ";
	cout << endl;
}
void ProcessEm(Event_t mode, clasEvent &evt, int debug) {
	clasParticle Em = evt.cp(Electron, 1);
	fourVec em = Em.p();
	fourVec beam=evt.beam().get4P(), target=evt.target().get4P();
	cout << "M39 ";
	ProcessGeneralHeader(evt, debug);
	cout << em.t() << " ";
	cout << em.V().r() << " " << em.V().cosTheta() << " " << em.V().phi()/M_PI << " ";
	cout << Em.ecEnergy() << " " << Em.ecTime() << " ";
	cout << endl;
}
void ProcessP(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec p = evt.cp(Proton,1).p();
	fourVec beam=evt.beam().get4P(), target=evt.target().get4P();


	int npip = evt.N(PiPlus), npim = evt.N(PiMinus), npi0 = evt.N(Pi0);

	// now plot masses

	cout << "M13 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - p).lenSq() << " ";
	cout << (beam + target - p).x() << " ";
	cout << (beam + target - p).y() << " ";
	cout << (beam + target - p).z() << " ";
	cout << (beam + target - p).r() << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/p;

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	p *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << p.cosTheta() << " " << p.phi()/M_PI << " ";
	processPhotons(evt, partbank);

	cout << endl;

}
void ProcessPim(Event_t mode, clasEvent &evt, int debug) {
	;
}
void ProcessPipPip(Event_t mode, clasEvent &evt, int debug) {
	;
}
void ProcessPip(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip = evt.cp(PiPlus,1).p();
	fourVec beam=evt.beam().get4P(), target=evt.target().get4P();

	int np = evt.N(Proton), npim = evt.N(PiMinus), npi0 = evt.N(Pi0);

	// now plot masses

	cout << "M14 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - pip).lenSq() << " ";
	cout << (beam + target - pip).x() << " ";
	cout << (beam + target - pip).y() << " ";
	cout << (beam + target - pip).z() << " ";
	cout << (beam + target - pip).r() << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	pip *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	pip *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/pip;

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	pip *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";
	processPhotons(evt, partbank);

	cout << endl;

}
int ProcessProtonProton(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec p1, p2;
	fourVec beam, target;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	p1 = evt.cp(Proton,1).p();
	p2 = evt.cp(Proton,2).p();

	// if debug is set, print proton 4-vectors

	if (debug) {

		cerr << "debug mode 11:\t" << evt.run() << "\t" << evt.event() << endl;
		cerr << beam;
		cerr << target;
		cerr << p1;
		cerr << p2;
		cerr << "D11 " << (beam + target - p1 - p2).lenSq() << " " << evt.z() << endl;
	}

	// now plot masses

	cout << "M11 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - p1 - p2).lenSq() << " ";
	cout << (beam + target - p1).lenSq() << " ";
	cout << (beam + target - p2).lenSq() << " ";
	cout << (beam + target - p1 - p2).x() << " ";
	cout << (beam + target - p1 - p2).y() << " ";
	cout << (beam + target - p1 - p2).z() << " ";
	cout << (beam + target - p1 - p2).r() << " ";

	cout << p1.x() << " " << p1.y() << " " << p1.z() << " " << p1.t() << " ";
	cout << p2.x() << " " << p2.y() << " " << p2.z() << " " << p2.t() << " ";

	// t to pbar p1
	cout << -(target - p2).lenSq() << " ";
	// t to pbar p2
	cout << -(target - p1).lenSq() << " ";
	// andle between p1 and p2

	cout << (p1.V() - p2.V()).cosTheta() << " ";

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p1 *= L;
	p2 *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	p1 *= L;
	p2 *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(p1+p2);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	p1 *= L;
	p2 *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (p1 + p2).cosTheta() << " " << (p1 + p2).phi()/M_PI << " ";
	processPhotons(evt, partbank);

	cout << endl;

	return (1);
}

int ProcessUnknown(Event_t mode, clasEvent &evt, int debug) {
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	part_t *part;
	int npip = 0, npim = 0, nKp = 0, nKm = 0, nneut = 0, nprot = 0, ngamma = 0;
	int nother = 0;
	if (PART) {
		for (int i = 0; i < PART->bank.nrow; ++i) {
			part = &PART->part[i];
			switch ((Particle_t) part->pid) {
			case PiPlus:
				npip++;
				break;
			case PiMinus:
				npim++;
				break;
			case KPlus:
				nKp++;
				break;
			case KMinus:
				nKm++;
				break;
			case Proton:
				nprot++;
				break;
			case Neutron:
				nneut++;
				break;
			case Gamma:
				ngamma++;
				break;
			default:
				nother++;
				break;
			}
		}
		cout << "M0 ";
		ProcessGeneralHeader(evt, debug);
		cout << npip << " " << npim << " " << nKp << " " << nKm << " " << nneut
				<< " " << nprot << " " << ngamma << " " << nother << " ";
		cout << endl;

	}
	return (1);
}
int ProcessPipPipPim(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip1, pim, pip2, neut;
	clasParticle PiP1, PiP2, PiM;
	fourVec beam, target;
	int npip = 0;
	int npim = 0;

	double t, tMin;

	beam = evt.getBeam(eBeamLo, eBeamHi);

	npip = evt.N(PiPlus);
	npim = evt.N(PiMinus);

	if (npip > 1 && npim) {

		PiP1 = evt.cp(PiPlus, 1);
		PiP2 = evt.cp(PiPlus, 2);
		PiM = evt.cp(PiMinus, 1);

		pip1 = PiP1.p();
		pip2 = PiP2.p();
		pim = PiM.p();

		target = evt.target().get4P();

		// now plot masses

		neut = beam + target - pip1 - pip2 - pim;

		cout << "M2 ";
		ProcessGeneralHeader(evt, debug);
		tMin = tmin(evt.beam().get4P().t(), 0.0, PROTON_MASS, ~(pip1 + pip2 + pim), NEUTRON_MASS);
		cout << (beam + target - pip2 - pip1 - pim).lenSq() << " ";
		cout << ~(pip1 + pim) << " ";
		cout << ~(pip1 + pip2) << " " << ~(pim + pip2) << " ";
		cout << ~(pip2 + pim + pip1) << " ";
		cout << (pim + neut).lenSq() << " ";
		cout << (pip1 + neut).lenSq() << " ";
		cout << (pip2 + neut).lenSq() << " ";
		t = (target - neut).lenSq();
		cout << PiP1.beta() - PiP1.Beta() << " " << PiP2.beta() - PiP2.Beta()
				<< " " << PiM.beta() - PiM.Beta() << " ";

		cout << PiP1.tofLength()/((PiP1.beta() - PiP1.Beta()) * LIGHT_SPEED)
				<< " " << PiP2.tofLength()/((PiP2.beta() - PiP2.Beta())
				* LIGHT_SPEED) << " " << PiM.tofLength()/((PiM.beta()
				- PiM.Beta()) * LIGHT_SPEED) << " ";

		cout << sqrt(neut.V().x() * neut.V().x() + neut.V().y() * neut.V().y()) << " ";
		cout << neut.V().z() << " ";
		cout << -t << " ";
		cout << -(t - tMin) << " ";
		cout << (pim.V()).r() << " " << (pip1.V()).r() << " " << (pip2.V()).r() << " ";

		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		pip2 *= L;
		pip1 *= L;
		pim *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		pip2 *= L;
		pip1 *= L;
		pim *= L;
		beam *= L;
		target *= L;

		//define the y-axis as the cross product of the 
		//beam and 3-pi system

		Cross = beam/(pip1+pim+pip2);

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		pip2 *= L;
		pip1 *= L;
		pim *= L;
		beam *= L;
		target *= L;

		// print these angles
		cout << (pim + pip1).cosTheta() << " " << (pim + pip1).phi()/M_PI << " ";

		// transform to 3pi restframe 
		L.set(pip1 + pip2 + pim);
		beam *= L;
		target *= L;
		pip2 *= L;
		pip1 *= L;
		pim *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		pip2 *= L;
		pip1 *= L;
		pim *= L;
		beam *= L;
		target *= L;

		// print out angles of pim
		cout << pim.cosTheta() << " " << pim.phi()/M_PI << " ";
		// print out angles of pip1
		cout << pip1.cosTheta() << " " << pip1.phi()/M_PI << " ";
		// print out angles of pip2
		cout << pip2.cosTheta() << " " << pip2.phi()/M_PI << " ";

		processPhotons(evt, partbank);
		cout << endl;
	}

	return (npip > 1 && npim);
}

int ProcessKpKmPip(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross, mp;
	clasParticle KP,KM,PIP;
	fourVec Kp, Km, pip, neut;
	fourVec beam, target;
	fourVec n; // n is constrained
	int npip = 0;
	int npim = 0;
	int nKplus = 0;
	int nKminus = 0;

	nKplus = evt.N(KPlus);
	nKminus = evt.N(KMinus);
	npip = evt.N(PiPlus);
	npim = evt.N(PiMinus);

	if (npip >= 1 && nKplus >= 1 && nKminus >= 1) {
		PIP = evt.cp(PiPlus,1);
		KP = evt.cp(KPlus,1);
		KM = evt.cp(KMinus,1);
		pip = evt.cp(PiPlus,1).p();
		Kp = evt.cp(KPlus,1).p();
		Km = evt.cp(KMinus,1).p();

		beam = evt.getBeam(eBeamLo, eBeamHi);
		target = evt.target().get4P();

		// now plot masses

		neut = beam + target - Kp - Km - pip;
		n = fourVec(sqrt(neut.V().lenSq() + NEUTRON_MASS * NEUTRON_MASS), neut.V());

		cout << "M6 ";
		ProcessGeneralHeader(evt, debug);
		cout << (beam + target - Kp - Km - pip).lenSq() << " ";
		cout << ~(Kp + Km) << " ";
		cout << ~(Km + pip) << " ";
		cout << ~(Kp + Km + pip) << " ";
		cout << ~(Kp + neut) << " ";
		cout << ~(Km + neut) << " ";
		cout << ~(pip + neut) << " ";
		cout << ~(Kp + n) << " ";
		cout << -(target - neut).lenSq() << " ";
		//    cout << (Kp.V()).r() << " " << (Km.V()).r() << " " << (pip.V()).r() << " ";
		mp = beam.V() + target.V() - Kp.V() - Km.V() - pip.V();
		cout << sqrt(mp.x()*mp.x() + mp.y() * mp.y()) << " " << mp.z() << " ";
		
		processTrack(KP);
		processTrack(KM);
		processTrack(PIP);

		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		Kp *= L;
		Km *= L;
		pip *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		Kp *= L;
		Km *= L;
		pip *= L;
		beam *= L;
		target *= L;

		//define the y-axis as the cross product of the 
		//beam and 3-pi system

		Cross = beam/(Km+pip+Kp);

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		Kp *= L;
		Km *= L;
		pip *= L;
		beam *= L;
		target *= L;

		// print these angles
		cout << (pip + Km).cosTheta() << " " << (pip + Km).phi()/M_PI << " ";
		cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";
		cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";
		cout << Km.cosTheta() << " " << Km.phi()/M_PI << " ";

		// transform to 3pi restframe 
		L.set(Km + Kp + pip);
		beam *= L;
		target *= L;
		Kp *= L;
		Km *= L;
		pip *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		Kp *= L;
		Km *= L;
		pip *= L;
		beam *= L;
		target *= L;

		// print out angles of pip
		cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

		processPhotons(evt, partbank);
		cout << endl;
	}

	return (npip > 1 && npim);
}

int ProcessKpKpPim(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross, mp;
	clasParticle KP1,KP2,PIM;
	fourVec Kp1, Kp2, pim,miss;
	fourVec beam, target;
	
	int npip = 0;
	int npim = 0;
	int nKplus = 0;
	int nKminus = 0;

	nKplus = evt.N(KPlus);
	nKminus = evt.N(KMinus);
	npip = evt.N(PiPlus);
	npim = evt.N(PiMinus);

	if (npim >= 1 && nKplus >= 2 && nKminus == 0) {
		PIM = evt.cp(PiMinus,1);
		KP1 = evt.cp(KPlus,1);
		KP2 = evt.cp(KPlus,2);
		pim = evt.cp(PiMinus,1).p();
		Kp1 = evt.cp(KPlus,1).p();
		Kp2 = evt.cp(KPlus,2).p();

		beam = evt.getBeam(eBeamLo, eBeamHi);
		target = evt.target().get4P();

		// now plot masses

		miss = beam + target - Kp1 - Kp2 - pim;
		

		cout << "M55 ";
		ProcessGeneralHeader(evt, debug);
		cout << (beam + target - Kp1 - Kp2 - pim).lenSq() << " ";
		cout << ~(Kp1 + Kp2) << " ";
		cout << ~(Kp1 + pim) << " ";
		cout << ~(Kp2 + pim) << " ";
		
		cout << ~(Kp1 + Kp2 + pim) << " ";
		cout << ~(Kp1 + miss) << " ";
		cout << ~(Kp2 + miss) << " ";
		cout << ~(pim + miss) << " ";
		
		cout << -(target - miss).lenSq() << " ";
		
		cout << sqrt(mp.x()*mp.x() + mp.y() * mp.y()) << " " << mp.z() << " ";
		
		processTrack(KP1);
		processTrack(KP2);
		processTrack(PIM);

		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		Kp1 *= L;
		Kp2 *= L;
		pim *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		Kp1 *= L;
		Kp2 *= L;
		pim *= L;

		
		beam *= L;
		target *= L;

		//define the y-axis as the cross product of the 
		//beam and 3-pi system

		Cross = beam/(Kp1+pim+Kp2);

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		Kp1 *= L;
		Kp2 *= L;
				pim *= L;
		beam *= L;
		target *= L;

		// print these angles
		cout << (pim + Kp1).cosTheta() << " " << (pim + Kp1).phi()/M_PI << " ";
		cout << (pim + Kp2).cosTheta() << " " << (pim + Kp2).phi()/M_PI << " ";
		cout << pim.cosTheta() << " " << pim.phi()/M_PI << " ";
		cout << Kp1.cosTheta() << " " << Kp1.phi()/M_PI << " ";
		cout << Kp2.cosTheta() << " " << Kp2.phi()/M_PI << " ";

		// transform to meson restframe 
		L.set(Kp1 + Kp2 + pim);
		beam *= L;
		target *= L;
		Kp1 *= L;
		Kp2 *= L;
		pim *= L;
		
		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		Kp1 *= L;
		Kp2 *= L;
		pim *= L;

		beam *= L;
		target *= L;

		// print out angles of pim
		cout << pim.cosTheta() << " " << pim.phi()/M_PI << " ";

		processPhotons(evt, partbank);
		cout << endl;
	}

	return (npip > 1 && npim);
}





int ProcessKpPipPim(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross, mp;
	fourVec Kp, pim, pip, neut, X;
	fourVec beam, target;
	fourVec Ks;

	int npip = 0;
	int npim = 0;
	int nKplus = 0;

	nKplus = (mode == DstKpPipPim) ? evt.N(KPlus)
			: (mode == DstKmPipPim) ? evt.N(KMinus) : 0;
	npip = evt.N(PiPlus);
	npim = evt.N(PiMinus);

	if (npip >= 1 && nKplus >= 1 && npim >= 1) {
		pip = evt.cp(PiPlus,1).p();
		Kp = (mode == DstKpPipPim) ? evt.cp(KPlus,1).p() : evt.cp(KMinus).p();
		pim = evt.cp(PiMinus,1).p();

		beam = evt.beam().get4P();
		target = evt.target().get4P();

		// now plot masses
		X = beam + target - Kp - pim - pip;

		Ks = fourVec(sqrt((pip.V() + pim.V()).lenSq() + KAON_ZERO_MASS * KAON_ZERO_MASS), pip.V()
				+ pim.V());
		neut = fourVec(sqrt(X.V().lenSq() + NEUTRON_MASS * NEUTRON_MASS), X.V());

		cout << "M" << (int) mode << " ";
		ProcessGeneralHeader(evt, debug);
		cout << (beam + target - Kp - pim - pip).lenSq() << " ";
		cout << (beam + target - Kp - Ks).lenSq() << " ";
		cout << (beam + target - pip - pim).lenSq() << " ";
		cout << (beam + target - Ks).lenSq() << " ";
		cout << ~(Kp + pim) << " ";
		cout << ~(pim + pip) << " ";
		cout << ~(Kp + pim + pip) << " ";
		cout << ~(Kp + neut) << " " << ~(Kp + X) << " ";
		cout << ~(pim + neut) << " " << ~(pim + X) << " ";
		cout << ~(pip + neut) << " " << ~(pip + X) << " ";
		cout << ~(Ks + neut) << " ";
		cout << ~(Ks + Kp) << " ";

		cout << -(target - neut).lenSq() << " " << -(target - X).lenSq() << " ";
		cout << -(beam - Ks).lenSq() << " ";
		//    cout << (Kp.V()).r() << " " << (pim.V()).r() << " " << (pip.V()).r() << " ";
		mp = beam.V() + target.V() - Kp.V() - pim.V() - pip.V();
		cout << sqrt(mp.x()*mp.x() + mp.y() * mp.y()) << " " << mp.z() << " ";

		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		Kp *= L;
		pim *= L;
		pip *= L;
		Ks *= L;
		neut *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		Kp *= L;
		pim *= L;
		pip *= L;
		beam *= L;
		Ks *= L;
		neut *= L;
		target *= L;

		//define the y-axis as the cross product of the 
		//beam and 3-pi system

		Cross = beam/(pim+pip+Kp);

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		Kp *= L;
		pim *= L;
		pip *= L;
		beam *= L;
		target *= L;
		Ks *= L;
		neut *= L;

		// print these angles
		cout << (pip + pim).cosTheta() << " " << (pip + pim).phi()/M_PI << " ";
		cout << Ks.cosTheta() << " " << Ks.phi()/M_PI << " ";

		// transform to 3pi restframe 
		L.set(pim + Kp + pip);
		beam *= L;
		target *= L;
		Kp *= L;
		pim *= L;
		pip *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		Kp *= L;
		pim *= L;
		pip *= L;
		beam *= L;
		target *= L;

		// print out angles of pip
		cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

		processPhotons(evt, partbank);
		cout << endl;
	}

	return (npip > 1 && npim);
}

int ProcessChargedPart(clasEvent &evt, int debug, int partbank) {
	for (int i = 0; i < evt.Ncp(); ++i) {
		clasParticle cpart = evt.cp(i + 1);
		if (abs(cpart.Q()) > 0.0) {
			clasSChit scHit;
			clasSThit stHit;
				int stIsHit = 0, scIsHit = 0;
			double pl = cpart.pathlenST_SC();
			double beta = (pl/(cpart.sc_time() - cpart.st_time()))/LIGHT_SPEED;
			
			if (cpart.isSChit()) {
				scHit = cpart.SChit();
				scIsHit = 1;
			}
			if (cpart.isSThit()) {
							stHit = cpart.SThit();
							stIsHit = 1;
			}
			
			if (stIsHit * scIsHit) {

			cout << "CPART " << cpart.pid() << " " << cpart.mass() << " "
					<< ~(cpart.p().V()) << " " << cpart.beta() << " "
					<< cpart.Beta()<< " " << cpart.Q() << " ";
			cout << cpart.pathlenST_SC() << " ";
			cout << cpart.sc_time() << " " << cpart.st_time() << " " << beta
					<< " ";
			cout << scHit.pathLength() << " " << stHit.pathLength() << " ";
			cout << cpart.stVtime() << " " << cpart.scVertexTime(PI_CHARGED_MASS) << " " << 
			cpart.scVertexTime(KAON_CHARGED_MASS) << " " << cpart.scVertexTime(PROTON_MASS)
			<< " " << evt.vtime() << " ";
			cout << endl;
			}
		}
	}
	return (1);
}
void ProcessKpKp(Event_t mode, clasEvent &evt, int debug) {
	fourVec Kp1, Kp2;
	clasParticle Kp1cp, Kp2cp;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	fourVec X;
	threeVec mp;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	Kp1 = evt.cp(KPlus,1).p();
	Kp2= evt.cp(KPlus,2).p();
	X = beam + target - Kp1 - Kp2;
	Kp1cp = evt.cp(KPlus, 1);
	Kp2cp = evt.cp(KPlus, 2);

	cout << "M28 ";

	ProcessGeneralHeader(evt, debug);

	// now plot masses


	cout << (beam + target - Kp1 - Kp2).lenSq() << " ";
	mp = beam.V() + target.V() - Kp1.V() - Kp2.V();
	cout << sqrt(mp.x()*mp.x() + mp.y() * mp.y()) << " " << mp.z() << " ";
	cout << ~(X + Kp1) << " " << ~(X + Kp2) << " ";
	cout << (beam - Kp1).lenSq() << " " << (beam - Kp2).lenSq() << " ";
	cout << Kp1.r() << " " << Kp1cp.beta() << " ";
	cout << Kp2.r() << " " << Kp2cp.beta() << " ";
	cout << Kp1cp.mass() << " " << Kp2cp.mass() << " ";
	processTrack(Kp1cp);
	processTrack(Kp2cp);
	cout << endl;
}
void ProcessPPipPimGamGam(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pim, p, X,gam1,gam2;
	fourVec beam4Vec,beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	beam4Vec = evt.getBeam(eBeamLo, eBeamHi);
	beam = beam4Vec;
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	pim = evt.cp(PiMinus,1).p();
	p = evt.cp(Proton,1).p();
	gam1 = evt.cp(Gamma,1).p();
	gam2 = evt.cp(Gamma,2).p();

	// now plot masses

	cout << "M54 ";
	ProcessGeneralHeader(evt, debug);
	cout << evt.N(Gamma) << " ";
	cout << (beam + target - p - pip - pim - gam1 - gam2).lenSq() << " ";
	cout << (beam + target - p - pip - pim).lenSq() << " ";
	cout << (beam + target - p).lenSq() << " ";
	cout << ~(pip + pim) << " ";
	cout << ~(gam1 + gam2) << " ";
	cout << ~(gam1 + gam2 + pip + pim) << " ";
	cout << ~(pip + p) << " " << ~(pim + p) << " ";
	cout << ~(p + pim + pip) << " ";
	// t
	cout << (target - p).lenSq() << " ";
	// t'
	cout << (target - p).lenSq() - tmin(beam.z(), M(Gamma), M(Proton),
			~(pip + p), M(Proton)) << " ";
	cout << (beam + target - p - pip - pim - gam1 - gam2).x() << " ";
	cout << (beam + target - p - pip - pim - gam1 - gam2).y() << " ";
	cout << (beam + target - p - pip - pim - gam1 - gam2).z() << " ";
	cout << (beam + target - p - pip - pim - gam1 - gam2).r() << " ";

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	gam1 *= L;
	gam2 *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;
	gam1 *= L;
	gam2 *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(pip+pim+gam1+gam2);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;
	gam1 *= L;
	gam2 *= L;

	// print these angles

	cout << (pim + pip + gam1 + gam2).cosTheta() << " " << (pim + pip).phi()/M_PI << " ";

	// Boost to rest frame of 2-pi system

	L.set(pip + pim);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;
	gam1 *= L;
	gam2 *= L;

	// Align beam along z 

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;
	gam1 *= L;
	gam2 *= L;

	// print pi+ angles

	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	// Now the helicity frame

	beam = beam4Vec;
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	p = evt.cp(Proton,1).p();
	pim = evt.cp(PiMinus,1).p();
	X = pip + pim + gam1 + gam2;
	gam1 = evt.cp(Gamma,1).p();
	gam2 = evt.cp(Gamma,2).p();

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;
	gam1 *= L;
	gam2 *= L;

	//define the y-axis normal to beam and X
	// target  and the recoil proton

	Cross = beam/X;

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;
	gam1 *= L;
	gam2 *= L;

	// I am now in the CM frame- define z axis to be in direction of X  

	L.set(X.phi(), X.theta(), 0.0);

	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;
	gam1 *= L;
	gam2 *= L;

	// boost into X rest frame

	L.set(X);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;
	gam1 *= L;
	gam2 *= L;

	// write out angles of the pi+
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	cout << endl;

}

void ProcessPPipPim(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pim, p, X;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	pim = evt.cp(PiMinus,1).p();
	p = evt.cp(Proton,1).p();

	// now plot masses

	cout << "M1 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - p - pip - pim).lenSq() << " ";
	cout << (beam + target - p).lenSq() << " ";
	cout << (beam + target - pip - pim).lenSq() << " ";	
	cout << ~(pip + pim) << " ";
	cout << ~(pip + p) << " " << ~(pim + p) << " ";
	cout << ~(p + pim + pip) << " ";
	// t
	cout << (target - p).lenSq() << " ";
	// t'
	cout << (target - p).lenSq() - tmin(beam.z(), M(Gamma), M(Proton),
			~(pip + p), M(Proton)) << " ";
	cout << (beam + target - p - pip - pim).x() << " ";
	cout << (beam + target - p - pip - pim).y() << " ";
	cout << (beam + target - p - pip - pim).z() << " ";
	cout << (beam + target - p - pip - pim).r() << " ";

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(pip+pim);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (pim + pip).cosTheta() << " " << (pim + pip).phi()/M_PI << " ";

	// Boost to rest frame of 2-pi system

	L.set(pip + pim);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// Align beam along z 

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// print pi+ angles

	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	// Now the helicity frame

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	p = evt.cp(Proton,1).p();
	pim = evt.cp(PiMinus,1).p();
	X = pip + pim;

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;

	//define the y-axis normal to beam and X
	// target  and the recoil proton

	Cross = beam/X;

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;

	// I am now in the CM frame- define z axis to be in direction of X  

	L.set(X.phi(), X.theta(), 0.0);

	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;

	// boost into X rest frame

	L.set(X);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	X *= L;

	// write out angles of the pi+
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	processPhotons(evt, partbank);
	cout << endl;

}

void ProcessKpPipPimx(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pim, Kp, X;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	pim = evt.cp(PiMinus,1).p();
	Kp = evt.cp(KPlus,1).p();

	// now plot masses

	cout << "M51 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - Kp - pip - pim).lenSq() << " ";
	cout << (beam + target - Kp).lenSq() << " ";
	cout << ~(pip + pim) << " ";
	cout << ~(pip + Kp) << " " << ~(pim + Kp) << " ";
	cout << ~(Kp + pim + pip) << " ";
	// t
	cout << (beam - (Kp + pip + pim)).lenSq() << " ";

	cout << (beam + target - Kp - pip - pim).x() << " ";
	cout << (beam + target - Kp - pip - pim).y() << " ";
	cout << (beam + target - Kp - pip - pim).z() << " ";
	cout << (beam + target - Kp - pip - pim).r() << " ";

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	Kp *= L;
	pip *= L;
	pim *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	Kp *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(pip+pim);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	Kp *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (pim + pip).cosTheta() << " " << (pim + pip).phi()/M_PI << " ";

	// Boost to rest frame of 2-pi system

	L.set(pip + pim);
	Kp *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// Align beam along z 

	L.set(beam.phi(), beam.theta(), 0.0);
	Kp *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// print pi+ angles

	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	// Now the helicity frame

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	Kp = evt.cp(KPlus,1).p();
	pim = evt.cp(PiMinus,1).p();
	X = pip + pim;

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	pip *= L;
	pim *= L;
	X *= L;

	//define the y-axis normal to beam and X
	// target  and the recoil proton

	Cross = beam/X;

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	beam *= L;
	target *= L;
	pip *= L;
	pim *= L;
	X *= L;

	// I am now in the CM frame- define z axis to be in direction of X  

	L.set(X.phi(), X.theta(), 0.0);

	beam *= L;
	target *= L;
	pip *= L;
	pim *= L;
	X *= L;

	// boost into X rest frame

	L.set(X);
	beam *= L;
	target *= L;
	pip *= L;
	pim *= L;
	X *= L;

	// write out angles of the pi+
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	processPhotons(evt, partbank);
	cout << endl;

}

void ProcessNPipPip(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip1, pip2, n;
	fourVec beam, target;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip1 = evt.cp(PiPlus,1).p();
	pip2 = evt.cp(PiPlus,2).p();
	n = evt.cp(Neutron,1).p();

	// now plot masses

	cout << "M20 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - n - pip1 - pip2).lenSq() << " ";
	cout << (beam + target - n).lenSq() << " ";
	cout << ~(pip1 + pip2) << " ";
	cout << ~(pip1 + n) << " " << ~(pip2 + n) << " ";
	cout << ~(n + pip1 + pip2) << " ";
	cout << (beam + target - n - pip1 - pip2).x() << " ";
	cout << (beam + target - n - pip1 - pip2).y() << " ";
	cout << (beam + target - n - pip1 - pip2).z() << " ";
	cout << (beam + target - n - pip1 - pip2).r() << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	n *= L;
	pip1 *= L;
	pip2 *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	n *= L;
	pip1 *= L;
	pip2 *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(pip1+pip2);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	n *= L;
	pip1 *= L;
	pip2 *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (pip1 + pip2).cosTheta() << " " << (pip1 + pip2).phi()/M_PI << " ";

	processPhotons(evt, partbank);

	cout << endl;

}

int ProcessPPipPimPi0(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip = evt.cp(PiPlus,1).p(), pim = evt.cp(PiMinus,1).p(), pi0 = evt.cp(Pi0,1).p(), p = evt.cp(Proton,1).p();
	fourVec beam=evt.beam().get4P(), target=evt.target().get4P();

	// now plot masses

	cout << "M9 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - p - pip - pim - pi0).lenSq() << " ";
	cout << (beam + target - p).lenSq() << " ";
	cout << ~(pip + pim + pi0) << " ";
	cout << ~(pip + pim) << " ";
	cout << ~(pip + pi0) << " ";
	cout << ~(pi0 + pim) << " ";
	cout << ~(pip + p) << " " << ~(pim + p) << " ";
	cout << ~(pi0 + p) << " ";
	cout << ~(p + pim + pip + pi0) << " ";
	cout << (beam + target - p - pip - pim - pi0).x() << " ";
	cout << (beam + target - p - pip - pim - pi0).y() << " ";
	cout << (beam + target - p - pip - pim - pi0).z() << " ";
	cout << (beam + target - p - pip - pim - pi0).r() << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pim *= L;
	pi0 *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	pip *= L;
	pim *= L;
	pi0 *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 3-pi system

	Cross = beam/(pip+pim+pi0);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	p *= L;
	pip *= L;
	pim *= L;
	pi0 *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (pim + pip).cosTheta() << " " << (pim + pip).phi()/M_PI << " ";
	processPhotons(evt, partbank);

	cout << endl;
	return (1);
}

int ProcessGammas(Event_t mode, clasEvent &evt, int debug) {
	cout << "M10 ";
	ProcessGeneralHeader(evt, debug);
	processPhotons(evt, partbank);
	cout << endl;
	return (1);
}

int ProcessPPipPi0(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec pip = evt.cp(PiPlus,1).p(), pi0 = evt.cp(Pi0,1).p(), p = evt.cp(Proton,1).p();
	fourVec beam=evt.beam().get4P(), target=evt.target().get4P();
	// now plot masses

	if (debug) {
		beam.print();
		target.print();
		(p + pip + pi0).print();
	}

	cout << "M7 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - p - pip - pi0).lenSq() << " ";
	cout << (beam + target - p).lenSq() << " ";
	cout << ~(pip + pi0) << " ";
	cout << ~(pip + p) << " " << ~(pi0 + p) << " ";
	cout << ~(p + pi0 + pip) << " ";
	cout << (beam + target - p - pip).lenSq() << " ";
	cout << (beam + target - p - pi0).lenSq() << " ";
	cout << (beam + target - p - pip - pi0).x() << " ";
	cout << (beam + target - p - pip - pi0).y() << " ";
	cout << (beam + target - p - pip - pi0).z() << " ";
	cout << (beam + target - p - pip - pi0).r() << " ";
	cout << (target - p).lenSq() << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	pi0 *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	p *= L;
	pip *= L;
	pi0 *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(pip+pi0);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	p *= L;
	pip *= L;
	pi0 *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (pi0 + pip).cosTheta() << " " << (pi0 + pip).phi()/M_PI << " ";
	cout << endl;

	return (1);
}
int ProcessPKpPi0(Event_t mode, clasEvent &evt, int debug) {
	lorentzTransform L;
	threeVec Cross;
	fourVec kp, pi0, p;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	part_t *part;
	double beamp = -100.0;
	clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	if (TAGR && TBID) {
		beamp = get_photon_energy(TAGR, (clasBID_t *)TBID);
	} else if (TAGR) {
		beamp = TAGR->tagr[0].erg;
	}
	beam.set(beamp, threeVec(0.0, 0.0, beamp));

	if (PART) {
		for (int i = 0; i < PART->bank.nrow; ++i) {
			part = &PART->part[i];
			switch ((Particle_t) part->pid) {
			case Proton:
				p.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			case KPlus:
				kp.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			case Pi0:
				pi0.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			default:
				break;
			}
		}
		// now plot masses

		cout << "M8 ";
		ProcessGeneralHeader(evt, debug);
		cout << (beam + target - p - kp - pi0).lenSq() << " ";
		cout << (beam + target - p).lenSq() << " ";
		cout << ~(kp + pi0) << " ";
		cout << ~(kp + p) << " " << ~(pi0 + p) << " ";
		cout << ~(p + pi0 + kp) << " ";
		cout << (beam + target - p - kp - pi0).x() << " ";
		cout << (beam + target - p - kp - pi0).y() << " ";
		cout << (beam + target - p - kp - pi0).z() << " ";
		cout << (beam + target - p - kp - pi0).r() << " ";
		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		p *= L;
		kp *= L;
		pi0 *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		kp *= L;
		pi0 *= L;
		beam *= L;
		target *= L;

		//define the y-axis as the cross product of the 
		//beam and 2-pi system

		Cross = beam/(kp+pi0);

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		p *= L;
		kp *= L;
		pi0 *= L;
		beam *= L;
		target *= L;

		// print these angles

		cout << (pi0 + kp).cosTheta() << " " << (pi0 + kp).phi()/M_PI << " ";
		processPhotons(evt, partbank);

		cout << endl;

	}
	return (PART != NULL);
}

int ProcessPKpKm(Event_t mode, clasEvent &evt, int debug) {
	/* Mode 5 */
	lorentzTransform L;
	threeVec Cross;
	fourVec Kp, Km, p;
	clasParticle Kpx, Kmx, px;
	fourVec beam, target;
	fourVec X;
	int ret;

	if ( (ret = (evt.N(Proton) && evt.N(KPlus) && evt.N(KMinus))) ){

		beam = evt.getBeam(eBeamLo, eBeamHi);
		target = evt.target().get4P();

		Kpx = evt.cp(KPlus, 1);
		Kmx = evt.cp(KMinus, 1);
		px = evt.cp(Proton, 1);

		Kp = Kpx.p();
		Km = Kmx.p();
		p = px.p();

		X = beam + target - Kp - Km - p;

		// now plot masses

		cout << "M5 ";
		ProcessGeneralHeader(evt, debug);
		cout << (beam + target - p - Kp - Km).lenSq() << " ";
		cout << (beam + target - p).lenSq() << " ";
		cout << ~(Kp + Km) << " ";
		cout << ~(Kp + p) << " " << ~(Km + p) << " ";
		cout << ~(p + Km + Kp) << " ";
		cout << (beam + target - p - Km).lenSq() << " ";
		cout << (beam + target - p - Kp).lenSq() << " ";
		cout << (beam + target - Kp - Km).lenSq() << " ";
		cout << (beam + target - Kp).lenSq() << " ";
		cout << (beam + target - Km).lenSq() << " ";
		cout << (beam + target - p - Kp - Km).x() << " ";
		cout << (beam + target - p - Kp - Km).y() << " ";
		cout << (beam + target - p - Kp - Km).z() << " ";
		cout << (beam + target - p - Kp - Km).r() << " ";

		cout << evt.cp(Proton,1).beta() << " " << ~p.V() << " " << p.V().cosTheta() << " " << p.V().phi()/M_PI << " ";
		cout << evt.cp(KPlus,1).beta() << " " << ~Kp.V() << " "<< Kp.V().cosTheta() << " " << Kp.V().phi()/M_PI << " ";
		cout << evt.cp(KMinus,1).beta() << " " << ~Km.V() << " " << Km.V().cosTheta() << " " << Km.V().phi()/M_PI << " ";

		processTrack(Kpx);
		processTrack(Kmx);
		processTrack(px);

		
		
		// momentum transfers....

		cout << (beam - Kp - Km).lenSq() << " " << (beam - Kp - Km).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS, ~(Kp
				+ Km), PROTON_MASS) << " ";
		cout << (beam - Kp).lenSq() << " " << (beam - Kp).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS,
				KAON_CHARGED_MASS, ~(Km + p)) << " ";
		cout << (beam - Km).lenSq() << " " << (beam - Km).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS,
				KAON_CHARGED_MASS, ~(Kp + p)) << " ";

		cout << (beam - Kp - Km - X).lenSq() << " " << (beam - Kp - Km - X).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS,
				~(Kp + Km + X), PROTON_MASS) << " ";

		cout << Kpx.beta() - Kpx.Beta() << " " << Kmx.beta() - Kmx.Beta()
				<< " " << px.beta() - px.Beta() << " ";

		cout << Kpx.tofLength()/((Kpx.beta() - Kpx.Beta()) * LIGHT_SPEED)
				<< " " << Kmx.tofLength()/((Kmx.beta() - Kmx.Beta())
				* LIGHT_SPEED) << " " << px.tofLength()
				/((px.beta() - px.Beta()) * LIGHT_SPEED) << " ";

		cout << sqrt(X.x()*X.x() + X.y() * X.y()) << " " << X.z() << " ";

		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		p *= L;
		Kp *= L;
		Km *= L;
		X *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;

		//define the y-axis as the cross product of the 
		//target and recoil p

		Cross = target/p;

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;

		// print these angles

		cout << (Km + Kp).cosTheta() << " " << (Km + Kp).phi()/M_PI << " ";

		// cm angles:

		cout << p.V().r() << " " << p.cosTheta() << " " << p.phi()/M_PI << " ";
		cout << Kp.V().r() << " " << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";
		cout << Km.V().r() << " " << Km.cosTheta() << " " << Km.phi()/M_PI << " ";

		// Boost to rest frame of K K X


		L.set(Kp + Km + X);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;

		// Align beam along z 

		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;

		// print K+ K- angles and momentum
		cout << (Kp + Km).V().r() << " ";
		cout << (Kp + Km).cosTheta() << " " << (Kp + Km).phi()/M_PI << " ";

		// Boost to rest frame of 2-K system

		L.set(Kp + Km);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;

		// Align beam along z 

		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;

		// print K+ angles and momentum
		cout << Kp.V().r() << " ";
		cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";

		processPhotons(evt, partbank);

		cout << endl;
	}

	return (ret);
}

int ProcessKpKm(Event_t mode, clasEvent &evt, int debug) {
	/* Mode 24 */
	lorentzTransform L;
	threeVec Cross, mp;
	fourVec Kp, Km;
	clasParticle Kpcp, Kmcp;
	fourVec X;
	fourVec beam, target;
	int ret;

	beam = evt.getBeam(eBeamLo, eBeamHi);
	target = evt.target().get4P();
	Kpcp = evt.cp(KPlus, 1);
	Kmcp = evt.cp(KMinus, 1);
	Kp = evt.cp(KPlus,1).p();
	Km = evt.cp(KMinus,1).p();
	X = beam + target - Kp - Km;

	// now plot masses

	cout << "M24 ";
	ProcessGeneralHeader(evt, debug);
	cout << evt.N(Proton) << " ";
	cout << (beam + target - Kp - Km).lenSq() << " ";
	cout << (beam + target - Kp).lenSq() << " " << (beam + target - Km).lenSq() << " ";
	cout << ~(Kp + Km) << " ";
	mp = beam.V() + target.V() - Kp.V() - Km.V();
	cout << sqrt(mp.x()*mp.x() + mp.y() * mp.y()) << " " << mp.z() << " ";
	cout << Kp.V().cosTheta() << " " << Kp.V().phi()/M_PI << " " << Kp.V().r() << " ";
	cout << Km.V().cosTheta() << " " << Km.V().phi()/M_PI << " " << Km.V().r() << " ";
	
	processTrack(Kpcp);
	processTrack(Kmcp);

	
	
	cout << -(beam - Kp - Km).lenSq() << " ";
	cout << -(beam - Kp).lenSq() << " " << -(beam - Km).lenSq() << " ";
	cout << ~(Kp + X) << " " << ~(Km + X) << " ";
	cout << Kpcp.mass() << " " << Kmcp.mass() << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	Kp *= L;
	Km *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	Kp *= L;
	Km *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(Kp+Km);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);

	Kp *= L;
	Km *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (Km + Kp).cosTheta() << " " << (Km + Kp).phi()/M_PI << " ";

	// Boost to rest frame of 2-K system

	L.set(Kp + Km);
	Kp *= L;
	Km *= L;
	beam *= L;
	target *= L;

	// Align beam along z 

	L.set(beam.phi(), beam.theta(), 0.0);
	Kp *= L;
	Km *= L;
	beam *= L;
	target *= L;

	// print K+ angles and momemtum

	cout << Kp.V().r() << " ";

	cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";
	cout << endl;
	return (ret);
}

void ProcessPPip(Event_t mode, clasEvent &evt, int debug) {

	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, p;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	fourVec X;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	p = evt.cp(Proton,1).p();

	nminus = evt.N(-1);
	n0 = evt.N(0);
	nplus = evt.N(1);

	qtot = nplus - nminus;

	beam = evt.getBeam(4.4, 6.0);

	// now plot 

	X = beam + target - p;

	cout << "M3 ";
	ProcessGeneralHeader(evt, debug);
	cout << sqrt((beam + target).lenSq()) << " ";
	cout << (beam + target - p).lenSq() << " ";
	cout << (beam + target - p - pip).lenSq() << " ";
	cout << ~(pip + p) << " ";
	// print angle of p and pi+ in lab
	cout << p.cosTheta() << " " << p.phi()/M_PI << " ";
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	// print out momentum of p an pi+ in lab

	cout << p.r() << " " << pip.r() << " ";

	// Transform to CM system

	L.set(beam + target);
	if (L.beta() < 1.0) {
		beam *= L;
		target *= L;
		p *= L;
		pip *= L;
		X *= L;

		//define the y-axis normal to beam and X
		// target  and the recoil proton

		Cross = beam/X;

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		beam *= L;
		target *= L;
		p *= L;
		pip *= L;
		X *= L;

		// I am now in the CM frame: print out angles of the pi+, and the meson system

		cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";
		cout << X.cosTheta() << " " << X.phi()/M_PI << " ";

		// boost into X rest frame

		L.set(X);
		if (L.beta() < 1.0) {
			beam *= L;
			target *= L;
			p *= L;
			pip *= L;
			X *= L;

			// align beam along z-axis

			L.set(beam.phi(), beam.theta(), 0.0);

			beam *= L;
			target *= L;
			p *= L;
			pip *= L;
			X *= L;

			// write out angles of the pi+
			cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

			// Now the helicity frame

			beam = evt.beam().get4P();
			target = evt.target().get4P();
			pip = evt.cp(PiPlus,1).p();
			p = evt.cp(Proton,1).p();
			X = beam + target - p;

			// Transform to CM system

			L.set(beam + target);
			if (L.beta() < 1.0) {
				beam *= L;
				target *= L;
				p *= L;
				pip *= L;
				X *= L;

				//define the y-axis normal to beam and X
				// target  and the recoil proton

				Cross = beam/X;

				L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
				beam *= L;
				target *= L;
				p *= L;
				pip *= L;
				X *= L;

				// I am now in the CM frame- define z axis to be in direction of X  

				L.set(X.phi(), X.theta(), 0.0);
				beam *= L;
				target *= L;
				p *= L;
				pip *= L;
				X *= L;

				// boost into X rest frame

				L.set(X);
				if (L.beta() < 1.0) {
					beam *= L;
					target *= L;
					p *= L;
					pip *= L;
					X *= L;

					// write out angles of the pi+
					cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";
					cout << endl;
				} else
					cout << " -1000 -1000 ";
			} else
				cout << "-1000 -1000 ";
		} else
			cout << "-1000 -1000 -1000 -1000 ";
	} else
		cout << "-1000 -1000 -1000 -1000 -1000 -1000 ";

	cout << endl;
}

void ProcessPPim(Event_t mode, clasEvent &evt, int debug) {

	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pim, p;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	fourVec X;
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	part_t *part;
	double beamp = -100.0;
	clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	if (TAGR && TBID) {
		beamp = get_photon_energy(TAGR, (clasBID_t *)TBID);
	}
	beam.set(beamp, threeVec(0.0, 0.0, beamp));

	if (PART) {
		for (int i = 0; i < PART->bank.nrow; ++i) {
			part = &PART->part[i];
			switch ((Particle_t) part->pid) {
			case Proton:
				p.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			case PiMinus:
				pim.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			default:
				switch (Q((Particle_t) part->pid)) {
				case 1:
					qtot++;
					nplus++;
					break;
				case -1:
					qtot--;
					nminus++;
					break;
				case 0:
					n0++;
					break;
				}

				break;
			}
		}
		// now plot 

		X = beam + target - p;

		cout << "M15 ";
		ProcessGeneralHeader(evt, debug);
		cout << (beam + target - p).lenSq() << " ";
		cout << (beam + target - p - pim).lenSq() << " ";
		cout << ~(pim + p) << " ";
		// print angle of p and pi+ in lab
		cout << p.cosTheta() << " " << p.phi()/M_PI << " ";
		cout << pim.cosTheta() << " " << pim.phi()/M_PI << " ";

		// print out momentum of p an pi+ in lab

		cout << p.r() << " " << pim.r() << " ";

		// Transform to CM system

		L.set(beam + target);
		if (L.beta() < 1.0) {

			beam *= L;
			target *= L;
			p *= L;
			pim *= L;
			X *= L;

			//define the y-axis normal to beam and X
			// target  and the recoil proton

			Cross = beam/X;

			L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
			if (L.beta() < 1.0) {
				beam *= L;
				target *= L;
				p *= L;
				pim *= L;
				X *= L;

				// I am now in the CM frame: print out angles of the pi+, and the meson system

				cout << pim.cosTheta() << " " << pim.phi()/M_PI << " ";
				cout << X.cosTheta() << " " << X.phi()/M_PI << " ";

				// boost into X rest frame

				L.set(X);
				if (L.beta() < 1.0) {
					beam *= L;
					target *= L;
					p *= L;
					pim *= L;
					X *= L;

					// align beam along z-axis

					L.set(beam.phi(), beam.theta(), 0.0);
					if (L.beta() < 1.0) {

						beam *= L;
						target *= L;
						p *= L;
						pim *= L;
						X *= L;

						// write out angles of the pi+
						cout << pim.cosTheta() << " " << pim.phi()/M_PI << " ";
					} else
						cout << "-1000 -1000 ";

				} else
					cout << "-1000 -1000 ";
			} else
				cout << "-1000 -1000 -1000 -1000 -1000 -1000 ";
		} else
			cout << "-1000 -1000 -1000 -1000 -1000 -1000 ";

		cout << endl;
	}
}


int ProcessPKm(Event_t mode, clasEvent &evt, int debug) {
	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec Km, p, X;
	clasParticle Kmx,px;
	fourVec beam, target(PROTON_MASS, threeVec(0.0,0.0,0.0));
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	part_t *part;
	double beamp = -100.0;
	clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	if (TAGR && TBID) {
		beamp = get_photon_energy(TAGR, (clasBID_t *)TBID);
	}
	beam.set(beamp, threeVec(0.0, 0.0, beamp));

	if (PART) {
		for (int i = 0; i < PART->bank.nrow; ++i) {
			part = &PART->part[i];
			switch ((Particle_t) part->pid) {
			case Proton:
				p.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			case KMinus:
				Km.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			default:
				switch (Q((Particle_t) part->pid)) {
				case 1:
					qtot++;
					nplus++;
					break;
				case -1:
					qtot--;
					nminus++;
					break;
				case 0:
					n0++;
					break;
				}

				break;
			}
		}
		// now plot
		Kmx = evt.cp(KMinus,1);
		px = evt.cp(Proton,1);
		X = beam + target - p;
		cout << "M23  ";
		ProcessGeneralHeader(evt, debug);
		cout << (target - p).lenSq() << " ";
		cout << (beam + target - p).lenSq() << " ";
		cout << (beam + target - p - Km).lenSq() << " ";
		cout << ~(Km + p) << " ";
		// print angle of p and K+ in lab
		cout << p.cosTheta() << " " << p.phi()/M_PI << " ";
		cout << Km.cosTheta() << " " << Km.phi()/M_PI << " ";


		processTrack(Kmx);
		processTrack(px);
		// print out momentum of p an pi+ in lab

		cout << p.r() << " " << Km.r() << " ";

		// Transform to CM system

		L.set(beam + target);

		if (L.beta() < 1.0) {

			beam *= L;
			target *= L;
			p *= L;
			Km *= L;
			X *= L;

			//define the y-axis normal to beam and X
			// target  and the recoil proton

			Cross = beam/X;

			L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
			if (L.beta() < 1.0) {
				beam *= L;
				target *= L;
				p *= L;
				Km *= L;
				X *= L;

				// I am now in the CM frame: print out angles of the K+, and the meson system

				cout << Km.cosTheta() << " " << Km.phi()/M_PI << " ";
				cout << X.cosTheta() << " " << X.phi()/M_PI << " ";

				// boost into X rest frame

				L.set(X);
				if (L.beta() < 1.0) {
					beam *= L;
					target *= L;
					p *= L;
					Km *= L;
					X *= L;

					// align beam along z-axis

					L.set(beam.phi(), beam.theta(), 0.0);
					if (L.beta() < 1.0) {

						beam *= L;
						target *= L;
						p *= L;
						Km *= L;
						X *= L;

						// write out angles of the K+
						cout << Km.cosTheta() << " " << Km.phi()/M_PI << " ";

					} else
						cout << "-1000 -1000 ";
				} else
					cout << "-1000 -1000 ";
			} else
				cout << "-1000 -1000 -1000 -1000 -1000 -1000";
		} else
			cout << "-1000 -1000 -1000 -1000 -1000 -1000 ";

		processPhotons(evt, partbank);
		cout << endl;
	}
	return (1);
}


int ProcessPKp(Event_t mode, clasEvent &evt, int debug) {
  int nplus = 0, nminus = 0, n0 = 0;
  int qtot = 0;
  lorentzTransform L;
  threeVec Cross;
  fourVec Kp, p, X;
  clasParticle Kpx,px;
  fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
  clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
  part_t *part;
  double beamp = -100.0;
  clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
  clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
  if (TAGR && TBID) {
    beamp = get_photon_energy(TAGR, (clasBID_t *)TBID);
  }
  beam.set(beamp, threeVec(0.0, 0.0, beamp));

  if (PART) {
    for (int i = 0; i < PART->bank.nrow; ++i) {
      part = &PART->part[i];
      switch ((Particle_t) part->pid) {
      case Proton:
	p.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,part->p.space.z));
	break;
      case KPlus:
	Kp.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,part->p.space.z));
	break;
      default:
	switch (Q((Particle_t) part->pid)) {
	case 1:
	  qtot++;
	  nplus++;
	  break;
	case -1:
	  qtot--;
	  nminus++;
	  break;
	case 0:
	  n0++;
	  break;
	}

	break;
      }
    }
  }
			
	       

  // now plot
  Kpx = evt.cp(KPlus,1);
  px = evt.cp(Proton,1);
  X = beam + target - p;
  cout << "M4 ";
  ProcessGeneralHeader(evt, debug);
  cout << (target - p).lenSq() << " ";
  cout << (beam + target - p).lenSq() << " ";
  cout << (beam + target - p - Kp).lenSq() << " ";
  cout << ~(Kp + p) << " ";
  // print angle of p and K+ in lab
  cout << p.cosTheta() << " " << p.phi()/M_PI << " ";
  cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";

  processTrack(Kpx);
  processTrack(px);


  // print out momentum of p an pi+ in lab

  cout << p.r() << " " << Kp.r() << " ";

  // Transform to CM system

  L.set(beam + target);

  if (L.beta() < 1.0) {

    beam *= L;
    target *= L;
    p *= L;
    Kp *= L;
    X *= L;

    //define the y-axis normal to beam and X
    // target  and the recoil proton

    Cross = beam/X;

    L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
    if (L.beta() < 1.0) {
      beam *= L;
      target *= L;
      p *= L;
      Kp *= L;
      X *= L;

      // I am now in the CM frame: print out angles of the K+, and the meson system

      cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";
      cout << X.cosTheta() << " " << X.phi()/M_PI << " ";

      // boost into X rest frame

      L.set(X);
      if (L.beta() < 1.0) {
	beam *= L;
	target *= L;
	p *= L;
	Kp *= L;
	X *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	if (L.beta() < 1.0) {

	  beam *= L;
	  target *= L;
	  p *= L;
	  Kp *= L;
	  X *= L;

	  // write out angles of the K+
	  cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";

	} else
	  cout << "-1000 -1000 ";
      } else
	cout << "-1000 -1000 ";
    } else
      cout << "-1000 -1000 -1000 -1000 -1000 -1000";
  } else
    cout << "-1000 -1000 -1000 -1000 -1000 -1000 ";

  processPhotons(evt, partbank);
  cout << endl;

return (1);
}

int ProcessPKpKmPipPim(Event_t mode, clasEvent &evt, int debug) {
	/* Mode 57 */
	lorentzTransform L;
	threeVec Cross;
	fourVec Kp, Km, p, Pip, Pim;
	clasParticle Kpx, Kmx, px, Pipx, Pimx;
	fourVec beam, target;
	fourVec X;
	int ret;
	
	if ( (ret = (evt.N(Proton) && evt.N(KPlus) && evt.N(KMinus) && evt.N(PiPlus) && evt.N(PiMinus))) ){
		
		beam = evt.getBeam(eBeamLo, eBeamHi);
		target = evt.target().get4P();
		
		Kpx = evt.cp(KPlus, 1);
		Kmx = evt.cp(KMinus, 1);
		px = evt.cp(Proton, 1);
		Pimx = evt.cp(PiMinus, 1);
		Pipx = evt.cp(PiPlus, 1);
		
		Kp = Kpx.p();
		Km = Kmx.p();
		p = px.p();
		Pip = Pipx.p();
		Pim = Pimx.p();
		
		X = beam + target - Kp - Km - Pip - Pim - p;
		
		// now plot masses
		
		cout << "M57 ";
		
		//label 1-15
		ProcessGeneralHeader(evt, debug);
		
		
		//label 16-25
		//MM^2 3 combinatorial, 5 choose 3  
		cout << (beam + target - p - Kp - Km).lenSq() << " ";
		cout << (beam + target - p - Kp - Pip).lenSq() << " ";
		cout << (beam + target - p - Kp - Pim).lenSq() << " ";
		cout << (beam + target - p - Km - Pip).lenSq() << " ";
		cout << (beam + target - p - Km - Pim).lenSq() << " ";
		cout << (beam + target - p - Pip - Pim).lenSq() << " ";
		cout << (beam + target - Kp - Km - Pip).lenSq() << " ";
		cout << (beam + target - Kp - Km - Pim).lenSq() << " ";
		cout << (beam + target - Kp - Pip - Pim).lenSq() << " ";
		cout << (beam + target - Km - Pip - Pim).lenSq() << " ";
		
		//label 26-35
		//Mass 3 combinatorial
		cout << ~(p + Kp + Km) << " ";
		cout << ~(p + Kp + Pip) << " ";
		cout << ~(p + Kp + Pim) << " ";
		cout << ~(p + Km + Pip) << " ";
		cout << ~(p + Km + Pim) << " ";
		cout << ~(p + Pip + Pim) << " ";
		cout << ~(Kp + Km + Pip) << " ";
		cout << ~(Kp + Km + Pim) << " ";
		cout << ~(Kp + Pip + Pim) << " ";
		cout << ~(Km + Pip + Pim) << " ";
		
		//label 36-45
		//MM^2 2 combinatorial, 5 choose 2
		cout << (beam + target - p - Kp).lenSq() << " ";
		cout << (beam + target - p - Km).lenSq() << " ";
		cout << (beam + target - p - Pip).lenSq() << " ";
		cout << (beam + target - p - Pim).lenSq() << " ";
		cout << (beam + target - Kp - Km).lenSq() << " ";
		cout << (beam + target - Kp - Pip).lenSq() << " ";
		cout << (beam + target - Kp - Pim).lenSq() << " ";
		cout << (beam + target - Km - Pip).lenSq() << " ";
		cout << (beam + target - Km - Pim).lenSq() << " ";
		cout << (beam + target - Pip - Pim).lenSq() << " ";
		
		//label 46-55
		//Mass 2 combinatorial
		cout << ~(p + Kp) << " ";
		cout << ~(p + Km) << " ";
		cout << ~(p + Pip) << " ";
		cout << ~(p + Pim) << " ";
		cout << ~(Kp + Km) << " ";
		cout << ~(Kp + Pip) << " ";
		cout << ~(Kp + Pim) << " ";
		cout << ~(Km + Pip) << " ";
		cout << ~(Km + Pim) << " ";
		cout << ~(Pip + Pim) << " ";

		//label 56-57
		cout << (beam + target - p - Kp - Km - Pip - Pim).lenSq() << " ";
		cout << ~(p + Km + Kp + Pip + Pim) << " ";
		
		//label 58-59
		cout << (beam + target - Kp - Km - Pip - Pim).lenSq() << " ";
		cout << ~(Kp + Km + Pip + Pim) << " ";
		
		//label 60-64
		cout << (beam + target - p).lenSq() << " ";
		cout << (beam + target - Kp).lenSq() << " ";
		cout << (beam + target - Km).lenSq() << " ";
		cout << (beam + target - Pip).lenSq() << " ";
		cout << (beam + target - Pim).lenSq() << " ";
		
		//label 65-68
		//missing momentum
		cout << (beam + target - p - Kp - Km - Pip - Pim).x() << " "; 
		cout << (beam + target - p - Kp - Km - Pip - Pim).y() << " "; 
		cout << (beam + target - p - Kp - Km - Pip - Pim).z() << " "; 
		cout << (beam + target - p - Kp - Km - Pip - Pim).r() << " "; 
		
		//label 69-72
		cout << evt.cp(Proton,1).beta() << " " << ~p.V() << " " << p.V().cosTheta() << " " << p.V().phi()/M_PI << " ";
		//label 73-76
		cout << evt.cp(KPlus,1).beta() << " " << ~Kp.V() << " "<< Kp.V().cosTheta() << " " << Kp.V().phi()/M_PI << " ";
		//label 77-80
		cout << evt.cp(KMinus,1).beta() << " " << ~Km.V() << " " << Km.V().cosTheta() << " " << Km.V().phi()/M_PI << " ";
		//label 81-84
		cout << evt.cp(PiPlus,1).beta() << " " << ~Kp.V() << " "<< Kp.V().cosTheta() << " " << Kp.V().phi()/M_PI << " ";
		//label 85-88
		cout << evt.cp(PiMinus,1).beta() << " " << ~Km.V() << " " << Km.V().cosTheta() << " " << Km.V().phi()/M_PI << " ";
		
		processTrack(Kpx);
		processTrack(Kmx);
		processTrack(px);
		processTrack(Pipx);
		processTrack(Pimx);
		
		
		// momentum transfers....
		
		cout << (beam - Kp - Km).lenSq() << " " << (beam - Kp - Km).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS, ~(Kp
																												 + Km), PROTON_MASS) << " ";
		cout << (beam - Kp).lenSq() << " " << (beam - Kp).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS,
																		 KAON_CHARGED_MASS, ~(Km + p)) << " ";
		cout << (beam - Km).lenSq() << " " << (beam - Km).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS,
																		 KAON_CHARGED_MASS, ~(Kp + p)) << " ";
		
		cout << (beam - Kp - Km - X).lenSq() << " " << (beam - Kp - Km - X).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS,
																						   ~(Kp + Km + X), PROTON_MASS) << " ";
		
		cout << (beam - Kp - Km - Pip - Pim - X).lenSq() << " "; 
		cout << (beam - Kp - Km - Pip - Pim - X).lenSq() - tmin(beam.t(), 0.0, PROTON_MASS, ~(Kp + Km + Pip + Pim + X), PROTON_MASS) << " ";
		
		
		
		cout << Kpx.beta() - Kpx.Beta() << " " << Kmx.beta() - Kmx.Beta()
		<< " " << px.beta() - px.Beta() << " ";
		
		cout << Kpx.tofLength()/((Kpx.beta() - Kpx.Beta()) * LIGHT_SPEED)
		<< " " << Kmx.tofLength()/((Kmx.beta() - Kmx.Beta())
								   * LIGHT_SPEED) << " " << px.tofLength()
		/((px.beta() - px.Beta()) * LIGHT_SPEED) << " ";
		
		cout << sqrt(X.x()*X.x() + X.y() * X.y()) << " " << X.z() << " ";
		
		// Transform to CM system
		
		L.set(beam + target);
		beam *= L;
		target *= L;
		p *= L;
		Kp *= L;
		Km *= L;
		X *= L;
		
		// align beam along z-axis
		
		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;
		
		//define the y-axis as the cross product of the 
		//target and recoil p
		
		Cross = target/p;
		
		//move this to align with the the y-axis
		
		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;
		
		// print these angles
		
		cout << (Km + Kp).cosTheta() << " " << (Km + Kp).phi()/M_PI << " ";
		
		// cm angles:
		
		cout << p.V().r() << " " << p.cosTheta() << " " << p.phi()/M_PI << " ";
		cout << Kp.V().r() << " " << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";
		cout << Km.V().r() << " " << Km.cosTheta() << " " << Km.phi()/M_PI << " ";
		
		// Boost to rest frame of K K X
		
		
		L.set(Kp + Km + X);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;
		
		// Align beam along z 
		
		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;
		
		// print K+ K- angles and momentum
		//cout << (Kp + Km).V().r() << " ";
		//cout << (Kp + Km).cosTheta() << " " << (Kp + Km).phi()/M_PI << " ";
		
		// Boost to rest frame of 2-K system
		
		L.set(Kp + Km);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;
		
		// Align beam along z 
		
		L.set(beam.phi(), beam.theta(), 0.0);
		p *= L;
		Kp *= L;
		Km *= L;
		beam *= L;
		target *= L;
		X *= L;
		
		// print K+ angles and momentum
		//cout << Kp.V().r() << " ";
		//cout << Kp.cosTheta() << " " << Kp.phi()/M_PI << " ";
		
		processPhotons(evt, partbank);
		
		cout << endl;
	}
	
	return (ret);
}

int processTAGT(clasEvent &evt, int debug) {
	clasTAGT_t *TAGT = (clasTAGT_t *)getBank(&bcs_, "TAGT");
	clasST1_t *ST1 = (clasST1_t *) getBank(&bcs_, "ST1 ");
	clasTAGR_t *TAGR = (clasTAGR_t *) getBank(&bcs_, "TAGR");
	cout << "TAGT ";
	ProcessGeneralHeader(evt, debug);

	if (TAGT) {
		int minT = 1000;
		int maxT = -1000;
		cout << TAGT->bank.nrow << " ";
		for (int i = 0; i < TAGT->bank.nrow; ++i) {
			if (TAGT->tagt[i].id == 46)
				continue;
			minT = (TAGT->tagt[i].id < minT) ? TAGT->tagt[i].id : minT;
			maxT = (TAGT->tagt[i].id > maxT) ? TAGT->tagt[i].id : maxT;
		}
		cout << minT << " " << maxT << endl;
	} else
		cout << " 2000 2000 " << endl;

	if (ST1 && TAGR) {
		for (int i = 0; i < TAGR->bank.nrow; ++i) {
			for (int j = 0; j < ST1->bank.nrow; ++j) {
				cout << "ST ";
				ProcessGeneralHeader(evt, debug);
				cout << ST1->st1[j].time_1 << " " << ST1->st1[j].time_2 << " "
						<< TAGR->tagr[i].t_id << " " << TAGR->tagr[i].ttag
						<< " " << TAGR->tagr[i].tpho<< " "
					" " << TAGR->tagr[i].ttag - (ST1->st1[j].time_1
						+ ST1->st1[j].time_2)/2.0 << " " << TAGR->tagr[i].tpho
						- (ST1->st1[j].time_1 + ST1->st1[j].time_2)/2.0 << endl;
			}
		}
	}

	if (ST1 && TAGT) {
		for (int i = 0; i < TAGT->bank.nrow; ++i) {
			for (int j = 0; j < ST1->bank.nrow; ++j) {
				cout << "STT ";
				ProcessGeneralHeader(evt, debug);
				cout << (ST1->st1[j].time_1 + ST1->st1[j].time_2)/2.0 << " "
						<< (TAGT->tagt[i].tdcl + TAGT->tagt[i].tdcr)/2.0;
				cout << endl;
			}
		}
	}

	return (1);
}

int ProcessEvent(clasEvent &evt, unsigned int triggerMask, int silentMode,
		int cutMode, int debug) {


	clasHEAD_t *HEAD = (clasHEAD_t *)getBank(&bcs_, "HEAD");
	clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");

	float beamp = 0.0;
	if (HEAD) {

		if (evt.type() == SCALER_EVENT) {
			ProcessScaler(evt, silentMode);

		}

		else if (evt.type() == DATA_EVENT || evt.type() == -2 || evt.type()
				== -4) {

			if (triggerMask ? (triggerMask & HEAD->head[0].trigbits) : 1) {

				if (TAGR && TBID) {
					beamp = get_photon_energy(TAGR, (clasBID_t *)TBID);
				}

				if (!cutMode || processCut(cutMode, evt)) {

					// first classify the event


					if (!silentMode) {

						//    processTAGT();

						if (debug)
							evt.printParticles();

						if (isEvent(DstAll, evt))
							ProcessAll(DstAll, evt, debug);
						if (isEvent(DstUnknown, evt))
							ProcessUnknown(DstUnknown, evt, debug);
						if (isEvent(DstPPipPim, evt))
							ProcessPPipPim(DstPPipPim, evt, debug);
						if (isEvent(DstPPipPimGamGam,evt))
							ProcessPPipPimGamGam(DstPPipPim, evt, debug);
						if (isEvent(DstPipPipPim, evt))
							ProcessPipPipPim(DstPipPipPim, evt, debug);
						if (isEvent(DstPPip, evt))
							ProcessPPip(DstPPip, evt, debug);
						if (isEvent(DstKpKmPip, evt))
							ProcessKpKmPip(DstKpKmPip, evt, debug);
						if (isEvent(DstKpPipPim, evt))
							ProcessKpPipPim(DstKpPipPim, evt, debug);

						if (isEvent(DstPKpKm, evt))
							ProcessPKpKm(DstPKpKm, evt, debug);
						if (isEvent(DstKpKm, evt))
							ProcessKpKm(DstKpKm, evt, debug);
						if (isEvent(DstPKp, evt))
							ProcessPKp(DstPKp, evt, debug);
						if (isEvent(DstPKm, evt))
							ProcessPKm(DstPKp, evt, debug);
						if (isEvent(DstPPipPi0, evt))
							ProcessPPipPi0(DstPPipPi0, evt, debug);
						if (isEvent(DstPKpPi0, evt))
							ProcessPKpPi0(DstPKpPi0, evt, debug);
						if (isEvent(DstPPipPimPi0, evt))
							ProcessPPipPimPi0(DstPPipPimPi0, evt, debug);
						if (isEvent(DstGammas, evt))
							ProcessGammas(DstGammas, evt, debug);
						if (isEvent(DstProtonProton, evt))
							ProcessProtonProton(DstProtonProton, evt, debug);
						if (isEvent(DstPipPi0, evt))
							ProcessPipPi0(DstPipPi0, evt, debug);
						if (isEvent(DstPipPim, evt))
							ProcessPipPim(DstPipPim, evt, debug);
						if (isEvent(DstP, evt))
							ProcessP(DstP, evt, debug);
						if (isEvent(DstEp, evt))
							ProcessEp(DstEp, evt, debug);
						if (isEvent(DstEm, evt))
							ProcessEm(DstEm, evt, debug);

						if (isEvent(DstPip, evt))
							ProcessPip(DstPip, evt, debug);
						if (isEvent(DstPim, evt))
							ProcessPim(DstPim, evt, debug);
						if (isEvent(DstPipPip, evt))
							ProcessPipPip(DstPipPip, evt, debug);
						if (isEvent(DstPPim, evt))
							ProcessPPim(DstPPim, evt, debug);
						if (isEvent(DstNPipPip, evt))
							ProcessNPipPip(DstNPipPip, evt, debug);
						if (isEvent(DstKpKp, evt))
							ProcessKpKp(DstKpKp, evt, debug);
						if (isEvent(DstPPipGam, evt))
							ProcessPPipGam(DstPPipGam, evt, debug);
						if (isEvent(DstPPipPimGam, evt))
							ProcessPPipPimGam(DstPPipPimGam, evt, debug);
						if (isEvent(DstPipPimGam, evt))
							ProcessPipPimGam(DstPipPimGam, evt, debug);
						if (isEvent(DstPipPipPimGam, evt))
							ProcessPipPipPimGam(DstPipPipPimGam, evt, debug);
						if (isEvent(DstEpEm, evt))
							ProcessEpEm(DstEpEm, evt, debug);
						
						if (isEvent(DstPKpKmPipPim, evt))
							ProcessPKpKmPipPim(DstPKpKmPipPim, evt, debug);

						if (isEvent(DstKmPipPim, evt))
							ProcessKpPipPim(DstKmPipPim, evt, debug);

						ProcessChargedPart(evt, debug, partbank);
						ProcessVert(evt, debug);
						ProcessETIME(evt, debug);
						ProcessNeutrals(evt, debug, partbank);
					}

				}

			}
		}
	} else
		cout << "Unable to find head bank" << endl;
	return (1);
}

int processCut(int cutMode, clasEvent &evt) {
	int ret = 1;
	fourVec beam = evt.beam().get4P();
	switch (cutMode) {
	case 6:
		ret = beam.t() > 4.7 && beam.t() < 6.0;
		break;
	}
	return (ret);
}

void ProcessPipPi0(Event_t mode, clasEvent &evt, int debug) {
	int nproton = 0, npim = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pi0, X;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	clasPART_t *PART = (clasPART_t *)getGroup(&bcs_, "PART", partbank);
	part_t *part;
	double beamp = -100.0;
	clasTAGR_t *TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
	clasTBID_t *TBID = (clasTBID_t *)getBank(&bcs_, "TBID");
	if (TAGR && TBID && 0) {
		beamp = get_photon_energy(TAGR, (clasBID_t *)TBID);
	}
	if (TAGR) {
		beamp = TAGR->tagr[0].erg;
	}
	beam.set(beamp, threeVec(0.0, 0.0, beamp));

	if (PART) {
		for (int i = 0; i < PART->bank.nrow; ++i) {
			part = &PART->part[i];
			switch ((Particle_t) part->pid) {
			case PiPlus:
				pip.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;
			case Pi0:
				pi0.set(part->p.t, threeVec(part->p.space.x, part->p.space.y,
						part->p.space.z));
				break;

			case Proton:
				nproton++;
				break;
			case PiMinus:
				npim++;
				break;
			default:
				break;
			}
		}

		X = beam + target - pip - pi0;
		// now print

		cout << "M12 ";
		ProcessGeneralHeader(evt, debug);
		cout << nproton << " " << npim << " ";
		cout << (beam + target - pip - pi0).lenSq() << " ";
		cout << ~(pip + pi0) << " ";
		cout << ~(pip + X) << " " << ~(pi0 + X) << " ";
		cout << (beam + target - pip - pi0).x() << " ";
		cout << (beam + target - pip - pi0).y() << " ";
		cout << (beam + target - pip - pi0).z() << " ";
		cout << (beam + target - pip - pi0).r() << " ";
		// Transform to CM system

		L.set(beam + target);
		beam *= L;
		target *= L;
		X *= L;
		pip *= L;
		pi0 *= L;

		// align beam along z-axis

		L.set(beam.phi(), beam.theta(), 0.0);
		X *= L;
		pip *= L;
		pi0 *= L;
		beam *= L;
		target *= L;

		//define the y-axis as the cross product of the 
		//beam and 2-pi system

		Cross = beam/(pip+pi0);

		//move this to align with the the y-axis

		L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
		X *= L;
		pip *= L;
		pi0 *= L;
		beam *= L;
		target *= L;

		// print these angles

		cout << (pi0 + pip).cosTheta() << " " << (pi0 + pip).phi()/M_PI << " ";
		cout << endl;

	}

}

int ProcessPipPim(Event_t mode, clasEvent &evt, int debug) {

	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pim, X;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	fourVec proton;

	clasParticle Pip = evt.cp(PiPlus, 1);
	clasParticle Pim = evt.cp(PiMinus, 1);

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = Pip.p();
	pim = Pim.p();

	X = beam + target - pip - pim;
	proton.set(sqrt(X.V().lenSq() + PROTON_MASS * PROTON_MASS), X.V());
	// now print

	cout << "M16 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - pip - pim).lenSq() << " ";
	cout << -(beam - pip - pim).lenSq() << " ";
	cout << ~(pip + pim) << " ";
	cout << ~(pip + X) << " " << ~(pim + X) << " ";
	cout << ~(proton + pip) << " " << ~(proton + pim) << " ";
	cout << (beam + target - pip - pim).x() << " ";
	cout << (beam + target - pip - pim).y() << " ";
	cout << (beam + target - pip - pim).z() << " ";
	cout << (beam + target - pip - pim).r() << " ";
	cout << X.phi()/M_PI << " ";
	processTrack(Pip);
	processTrack(Pim);
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	X *= L;
	pip *= L;
	pim *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	X *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(pip+pim);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	X *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// transform to pi+ pi- rest frame 

	L.set(pip + pim);
	X *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	X *= L;
	pip *= L;
	pim *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (pip).cosTheta() << " " << (pip).phi()/M_PI << " ";
	cout << endl;
	return (1);

}

void ProcessEpEm(Event_t mode, clasEvent &evt, int debug) {

	lorentzTransform L;
	threeVec Cross;
	fourVec ep, em, X;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));
	double tMin, t;

	clasParticle cpep = evt.cp(Positron, 1);
	clasParticle cpem = evt.cp(Electron, 1);

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	ep = evt.cp(Positron,1).p();
	em = evt.cp(Electron,1).p();

	tMin = tmin(evt.beam().get4P().t(), 0.0, PROTON_MASS, ~(ep + em), PROTON_MASS);
	t = (beam - ep - em).lenSq();
	X = beam + target - ep - em;
	// now print


	cout << "M" << (int)mode << " ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam + target - ep - em).lenSq() << " ";
	cout << -t << " " << -(t - tMin) << " ";
	cout << ep.V().r() << " " << ep.V().cosTheta() << " " << ep.V().phi()/M_PI << " ";
	cout << cpep.scVertexTime(ELECTRON_MASS) << " "
			<< cpep.scVertexTime(PI_CHARGED_MASS) << " ";
	cout << cpep.scPathLen() << " ";
	cout << em.V().r() << " " << em.V().cosTheta() << " " << em.V().phi()/M_PI << " ";
	cout << cpem.scVertexTime(ELECTRON_MASS) << " "
			<< cpem.scVertexTime(PI_CHARGED_MASS) << " ";
	cout << cpem.scPathLen() << " ";

	cout << ~(ep + em) << " ";
	cout << (ep + em).V().r() << " ";
	cout << (ep + em).V().r()/(ep+em).t() << " ";
	cout << ~(ep + X) << " " << ~(em + X) << " ";

	cout << ep.V() * em.V()/(ep.V().r() * em.V().r());

	cout << (beam + target - ep - em).x() << " ";
	cout << (beam + target - ep - em).y() << " ";
	cout << (beam + target - ep - em).z() << " ";
	cout << (beam + target - ep - em).r() << " ";
	cout << X.phi()/M_PI << " ";
	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	X *= L;
	ep *= L;
	em *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	X *= L;
	ep *= L;
	em *= L;
	beam *= L;
	target *= L;

	//define the y-axis as the cross product of the 
	//beam and 2-pi system

	Cross = beam/(ep+em);

	//move this to align with the the y-axis

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	X *= L;
	ep *= L;
	em *= L;
	beam *= L;
	target *= L;

	// transform to pi+ pi- rest frame 

	L.set(ep + em);
	X *= L;
	ep *= L;
	em *= L;
	beam *= L;
	target *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);
	X *= L;
	ep *= L;
	em *= L;
	beam *= L;
	target *= L;

	// print these angles

	cout << (ep).cosTheta() << " " << (ep).phi()/M_PI << " ";
	cout << endl;

}

void ProcessAll(Event_t mode, clasEvent &evt, int debug) {
	static int n = 0;

	cout << "ALL ";
	ProcessGeneralHeader(evt, debug);
	cout << ++n << " ";
	cout << endl << flush;
}
void ProcessETIME(clasEvent &evt, int debug) {
	clasTGPB_t *TGPB = (clasTGPB_t *) getBank(&bcs_, "TGPB");
	clasTBID_t *TBID = (clasTBID_t *) getBank(&bcs_, "TBID");
	if (TGPB && TBID) {
		float qtime = -1000.0;
		tbid_t *tbid;
		cout << "ETIME ";
		for (int k = 0; k < TBID->bank.nrow; ++k) {
			tbid = &TBID->tbid[k];
			if (tbid->track) {
				qtime = tbid->vtime;
				break;
			}
		}
		ProcessGeneralHeader(evt, debug);
		cout << TGPB->tgpb[0].time << " " << TGPB->tgpb[0].dt << " " << qtime
				<< " ";
		cout << evt.beamTime() << " ";

		cout << endl << flush;
	}
}

void ProcessVert(clasEvent &evt, int debug) {
	clasMVRT_t *MVRT = (clasMVRT_t *) getBank(&bcs_, "MVRT");
	clasVERT_t *VERT = (clasVERT_t *) getBank(&bcs_, "VERT");
	if (MVRT) {
		cout << "MVRT ";
		ProcessGeneralHeader(evt, debug);
		cout << MVRT->bank.nrow << " " << evt.x()<< " " << evt.y() << " "
				<< evt.z() << " ";
		cout << endl;
	}
	if (VERT) {
		cout << "VERT ";
		ProcessGeneralHeader(evt, debug);
		cout << VERT->bank.nrow << " " << VERT->vert[0].vert.x << " "
				<< VERT->vert[0].vert.y << " " << VERT->vert[0].vert.z << " ";
		cout << endl;
	}
}
int StartRun(int runNo) {
	int static CurrentRun = -1;
	if (CurrentRun != runNo) {
		vertex_brun(runNo);
		tagtcl_set_def_();
		CurrentRun = runNo;
	}
	return 0;
}

int EndRun(int run) {

	return 0;
}

int GetDispatcherCommand() {
	int ret;
	int maxSend = 2;
	int OkToSendData = 1;
	int WaitForALLFINISHED = 0;

	/* wait for command from Dispatcher */

	fd_set rset;
	fd_set wset;
	struct timeval timeout;
	int maxfd = 0;

	FD_ZERO(&rset);
	FD_ZERO(&wset);

	FD_SET(disIO_socket,&rset);
	if (disIO_socket > maxfd)
		maxfd = disIO_socket;

	if (OkToSendData && (requestedRawData > 0)) {
		FD_SET(disIO_socket,&wset);
		if (disIO_socket > maxfd)
			maxfd = disIO_socket;
	}

	timeout.tv_sec = 1;
	timeout.tv_usec = 0;

	if (OkToSendData && (requestedRawData > 0)) {
		timeout.tv_sec = 0;
		timeout.tv_usec = 0;
	}

	ret = select(maxfd + 1, &rset, &wset, NULL, &timeout);
	if (ret < 0) {
		cerr << "DisFeedDAQ: Error: select() returned " << ret << "errno: "
				<< errno << " " << strerror(errno) << endl;
		//	  exitFlag = 1;
		exit(0);
	}

	/* ok, we got a command. Now parse it */
	static char *msg= NULL;
	static int msglen = 0;
	char *cmd0;
	char *word;

	if (msg)
		free(msg);

	msg = NULL;

	ret = disIO_readRAW_alloc((void **)&msg, &msglen, 0);

	if (msg) {

		word = strtok(msg, ":");
		cmd0 = word;
		if (word) {
			cerr << "COMMAND: " << cmd0 << endl;

			if (strcmp(word, "NOP") == 0) {
				/* do nothing */
			} else if (strcmp(word, "PING") == 0) {
				printf("DisFeedDAQ: Command from Dispatcher: %s\n", word);
				disIO_command("PING-REPLY");
			} else if (strcmp(word, "REQUEST_DATA") == 0) {
				int imore;

				/* fprintf(stderr,"DisFeedDAQ: Command from Dispatcher: %s\n",word); */

				word = strtok(NULL,":");

				if (word)
					imore = strtoul(word, NULL, 0);
				else
					imore = 1;

				/* printf("REQUEST_DATA: more: %d, requested events: %d, sent: %d\n",imore,requestedRawData,isent); */

				requestedRawData += imore;
			} else if (strcmp(word, "MAXSEND") == 0) {
				cerr << "DisFeedDAQ: Command from Dispatcher: " << word << endl;

				word = strtok(NULL,":");

				if (word)
					maxSend = strtoul(word, NULL, 0);

				printf("DisFeedDAQ: New maxSend is %d\n", maxSend);
			} else if (strcmp(word, "ALLFINISHED") == 0) {
				if (WaitForALLFINISHED) {
					cerr << "DisFeedDAQ: Command ALLFINISHED from Dispatcher: "
							<< word << endl;
					//	    SendBeginTape(runNo);
					OkToSendData = 1;
					WaitForALLFINISHED = 0;
				} else {
					fprintf(stderr,"DisFeedDAQ: Unexpected command ALLFINISHED from Dispatcher was ignored.\n");
				}
			} else if (strcmp(word, "QUIT") == 0) {
				fprintf(stderr,"DisFeedDAQ: Command QUIT from Dispatcher: %s\n",word);
				exit(0);
			} else if (strcmp(word, "EXIT") == 0) {
				fprintf(stderr,"DisFeedDAQ: Command EXIT from Dispatcher: %s\n",word);
				exit(0);
			} else {
				fprintf(stderr,"DisFeedDAQ: Unexpected command from Dispatcher: [%s] was ignored.\n",msg);
			}
		}
	} else {
		cerr << "Received empty message from the Dispatcher" << endl;
	}

	return (ret);
}
extern "C" {
int ir_isnan_(float *x) {
	return (0);
}

int ir_isinf_(float *x) {

	return (0);
}

}

int printGeneralLabels(char *c) {
	char *intro = "";
	int ret = 0;
	int print = !(*c);
	if (matlab)
		intro = "%\t";
	if (print || (strcmp(c, "ALL") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for ALL " << "(" << ModeName(DstAll) << "):"
				<< endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tn {n}" << endl;
		ret = 1;
	}
	if (print || (strcmp(c, "PART") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for PART:" << endl;
		cout << intro << ++nlab << "\tpid {pid}" << endl;
		cout << intro << ++nlab << "\tq {q}" << endl;
		cout << intro << ++nlab << "\tp {p}" << endl;
		cout << intro << ++nlab << "\tbeta {beta}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "ETIME") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for ETIME:" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tTGPB interaction time {it}" << endl;
		cout << intro << ++nlab << "\tTGPB delta time {dt}" << endl;
		cout << intro << ++nlab << "\tTBID time {t}" << endl;
		cout << intro << ++nlab << "\tbeam time {rf}" << endl;
		cout << intro << ++nlab << "\tz {z}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "M0") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M0 (unknown):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\t# pi+ {npip}" << endl;
		cout << intro << ++nlab << "\t# pi- {npim}" << endl;
		cout << intro << ++nlab << "\t# K+ {nkp}" << endl;
		cout << intro << ++nlab << "\t# K- {nkm}" << endl;
		cout << intro << ++nlab << "\t# neutrons {nneut}" << endl;
		cout << intro << ++nlab << "\t# protons {nprot}" << endl;
		cout << intro << ++nlab << "\t# gammas {ngamma}" << endl;
		cout << intro << ++nlab << "\t# other {nother}" << endl;
		ret = 1;
	}
	if (print || (strcmp(c, "CPART") == 0)) {
		int nlab = 0;
		cout << intro << ++nlab << "\tpid {pid}" << endl;
		cout << intro << ++nlab << "\tmass {m}" << endl;
		cout << intro << ++nlab << "\tp {p}" << endl;
		cout << intro << ++nlab << "\tbeta {beta}" << endl;
		cout << intro << ++nlab << "\tBeta {Beta}" << endl;
		cout << intro << ++nlab << "\tcharge {q}" << endl;
		cout << intro << ++nlab << "\tst-sc pathlen {pl}" << endl;
		cout << intro << ++nlab << "\tsc time {sctime}" << endl;
		cout << intro << ++nlab << "\tst time {sttime}" << endl;
		cout << intro << ++nlab << "\tBETA  {b} " << endl;
		cout << intro << ++nlab << "\tsc pathlen  {scpl} " << endl;
		cout << intro << ++nlab << "\tst pathlen {stpl} " << endl;
		cout << intro << ++nlab << "\tst vtime {stvtime}" << endl;
		cout << intro << ++nlab << "\tsc vtime pi {scvpi}" << endl;
		cout << intro << ++nlab << "\tsc vtime K  {scvK} " << endl;
		cout << intro << ++nlab << "\tsc vtime proton  {scvp} " << endl;
		cout << intro << ++nlab << "\tvtime {vtime} " << endl;
		ret = 1;
	}
	if (print || (strcmp(c, "M1") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M1 (p pi+ pi-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p pi+ pi- {mm2ppippim}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/pi+ pi- {mm2pippim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi-) {mppim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi- pi+) {mppippim}" << endl;
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tt' {tprime}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM pi pi {ct}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM pi pi {phi}" << endl;
		cout << intro << ++nlab << "\tcos(theta) GJ pi+ {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI GJ pi+ {phigj}" << endl;
		cout << intro << ++nlab << "\tcos(theta) helicity pi+ {cth}" << endl;
		cout << intro << ++nlab << "\tphi/PI helicity pi+ {phih}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}
	if (print || (strcmp(c, "M2") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M2 (pi+ pi+ pi-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/pi+ pi+ pi- {mm2pipipi}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpip1pim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi+) {mpippip}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpip2pim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi+ pi-) {mpipipi}" << endl;
		cout << intro << ++nlab << "\tMass^2 (n pi-) {m2npim}" << endl;
		cout << intro << ++nlab << "\tMass^2 (n pi+1) {m2npip1}" << endl;

		cout << intro << ++nlab << "\tMass^2 (n pi+2) {m2npip2}" << endl;
		cout << intro << ++nlab << "\tdelta beta pip1 {dbp1}" << endl;
		cout << intro << ++nlab << "\tdelta beta pip2 {dbp2}" << endl;
		cout << intro << ++nlab << "\tdelta beta pim {dbpm}" << endl;
		cout << intro << ++nlab << "\tdelta pion time pip1 {dtp1}" << endl;
		cout << intro << ++nlab << "\tdelta pion time pip2 {dtp2}" << endl;
		cout << intro << ++nlab << "\tdelta pion time pim {dtpm}" << endl;
		cout << intro << ++nlab << "\tmissing pt {m_pt}" << endl;
		cout << intro << ++nlab << "\tmissing pz {m_pz}" << endl;
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tt' {tp}" << endl;
		cout << intro << ++nlab << "\tp(pi-) {ppim}" << endl;
		cout << intro << ++nlab << "\tp(pi+1) {ppip1}" << endl;
		cout << intro << ++nlab << "\tp(pi+2) {ppip2}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {cost}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phi}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J  pi- {cosgjm}" << endl;
		cout << intro << ++nlab << "\tphi/PI T-Y pi- {phitym}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J pi+1 {cosgjp1}" << endl;
		cout << intro << ++nlab << "\tphi/PI T-Y pi+1 {phityp1}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J pi+2 {cosgjp2}" << endl;
		cout << intro << ++nlab << "\tphi/PI T-Y pi+2 {phityp2}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}

	if (print || (strcmp(c, "M3") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M3 (proton + pi+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/(p pi+) {mm2ppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab p {ctlabp}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab p {philabp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab pi+ {ctlabpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab pi+ {philabpip}" << endl;
		cout << intro << ++nlab << "\tp (proton) Lab  {pp}" << endl;
		cout << intro << ++nlab << "\tp (pi+) Lab {ppip}" << endl;

		cout << intro << ++nlab << "\tcos(theta) CM pi+ {ctcmpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM pi+ {phicmpip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM X {ctcmx}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM X {phicmx}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J pi+ {ctgjpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI G-J pi+ {phigjpip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) helicity  pi+ {cthpip}"
				<< endl;
		cout << intro << ++nlab << "\tphi/PI helicity pi+ {phihpip}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "M4") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M4 (proton + K+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K+) {mm2pKp}" << endl;
		cout << intro << ++nlab << "\tMass(p K+) {mpKp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab p {coslab}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab p {philab}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab K+ {cosKp}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab K+ {phiKp}" << endl;

		cout << intro;
		printTrackLabels(&nlab,"Kp","kp");
	
		cout << intro;
		printTrackLabels(&nlab,"P","p");



		cout << intro << ++nlab << "\tp (proton) Lab  {pp}" << endl;
		cout << intro << ++nlab << "\tp (K+) Lab {pKp}" << endl;

		cout << intro << ++nlab << "\tcos(theta) CM K+ {ctcmKp}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM K+ {phicmKp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM X {ctcmx}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM X {phicmx}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J K+ {ctgjKp}" << endl;
		cout << intro << ++nlab << "\tphi/PI G-J K+ {phigjKp}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
	}
	if (print || (strcmp(c, "M23") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M23 (proton + K-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K-) {mm2pKm}" << endl;
		cout << intro << ++nlab << "\tMass(p K-) {mpKm}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab p {coslab}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab p {philab}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab K- {cosKm}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab K- {phiKm}" << endl;

		cout << intro;
		printTrackLabels(&nlab,"Km","km");
	
		cout << intro;
		printTrackLabels(&nlab,"P","p");



		cout << intro << ++nlab << "\tp (proton) Lab  {pp}" << endl;
		cout << intro << ++nlab << "\tp (K-) Lab {pKm}" << endl;

		cout << intro << ++nlab << "\tcos(theta) CM K- {ctcmKm}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM K- {phicmKm}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM X {ctcmx}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM X {phicmx}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J K- {ctgjKm}" << endl;
		cout << intro << ++nlab << "\tphi/PI G-J K- {phigjKm}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
	}
	
	if (print || (strcmp(c, "M5") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M5 (p K+ K-):" << endl;
		printGeneralHeaderLabels(&nlab);

		cout << intro << ++nlab << "\tMM^2/p K+ K- {mm2pkk}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K-) {mkk}" << endl;
		cout << intro << ++nlab << "\tMass(p K+) {mpKp}" << endl;
		cout << intro << ++nlab << "\tMass(p K-) {mpKm}" << endl;
		cout << intro << ++nlab << "\tMass(p K- K+) {mpkk}" << endl;
		cout << intro << ++nlab << "\tMM2/(p K-) {mm2pkm}" << endl;
		cout << intro << ++nlab << "\tMM2/(p K+) {mm2pkp}" << endl;

		cout << intro << ++nlab << "\tMM2/(K+ K-) {mm2kk}" << endl;
		cout << intro << ++nlab << "\tMM2/(K+) {mm2kp}" << endl;
		cout << intro << ++nlab << "\tMM2/(K-) {mm2km}" << endl;

		cout << intro << ++nlab << "\tmissing px {mpx}" << endl;
		cout << intro << ++nlab << "\tmissing py {mpy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {mpz}" << endl;
		cout << intro << ++nlab << "\tmissing p {mp}" << endl;

		cout << intro << ++nlab << "\tp beta {pb}" << endl;
		cout << intro << ++nlab << "\tp p {pp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab p {ctp}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab p {phip}" << endl;

		cout << intro << ++nlab << "\tK+ beta {kpb}" << endl;
		cout << intro << ++nlab << "\tK+  p {kpp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab K+ {ctkp}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab K+ {phikp}" << endl;

		cout << intro << ++nlab << "\tK- beta {kmb}" << endl;
		cout << intro << ++nlab << "\tK-  p {kmp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab K- {ctkm}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab K- {phikm}" << endl;

		cout << intro;
		printTrackLabels(&nlab,"Kp","kp");
		cout << intro;
		printTrackLabels(&nlab,"Km","km");
		cout << intro;
		printTrackLabels(&nlab,"P","p");
				
		
		cout << intro << ++nlab << "\tt to K+ K- {t}" << endl;
		cout << intro << ++nlab << "\tt' to K+ K- {tp}" << endl;

		cout << intro << ++nlab << "\tt to K+  {tkp}" << endl;
		cout << intro << ++nlab << "\tt' to K+  {tpkp}" << endl;

		cout << intro << ++nlab << "\tt to K-  {tkm}" << endl;
		cout << intro << ++nlab << "\tt' to K-  {tpkm}" << endl;

		cout << intro << ++nlab << "\tt to K+ K- X {tkpkmX}" << endl;
		cout << intro << ++nlab << "\tt' to K+ K- X  {tpkpkmX}" << endl;

		cout << intro << ++nlab << "\tdelta beta K+ {dbKp}" << endl;
		cout << intro << ++nlab << "\tdelta beta K- {dbKm}" << endl;
		cout << intro << ++nlab << "\tdelta beta proton {dbp}" << endl;
		cout << intro << ++nlab << "\tdelta K+ time  {dtKp}" << endl;
		cout << intro << ++nlab << "\tdelta K- time  {dtKm}" << endl;
		cout << intro << ++nlab << "\tdelta proton time {dtp}" << endl;
		cout << intro << ++nlab << "\tmissing pt {m_pt}" << endl;
		cout << intro << ++nlab << "\tmissing pz {m_pz}" << endl;

		cout << intro << ++nlab << "\tcos(theta) CM {ct}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phi}" << endl;

		cout << intro << ++nlab << "\tp of p CM {ppcm}" << endl;
		cout << intro << ++nlab << "\tcos theta p CM {ctpcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI p CM {phipcm}" << endl;

		cout << intro << ++nlab << "\tp of K+ CM {pKpcm}" << endl;
		cout << intro << ++nlab << "\tcos theta K+ CM {ctKpcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI K+ CM {phiKpcm}" << endl;

		cout << intro << ++nlab << "\tp of K- CM {pKmcm}" << endl;
		cout << intro << ++nlab << "\tcos theta K- CM {ctKmcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI K- CM {phiKmcm}" << endl;

		cout << intro << ++nlab << "\tp in KK system  {pkk}" << endl;

		cout << intro << ++nlab << "\tcos(theta) GJ (K+K-) {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI GJ (K+K-) {phigj}" << endl;

		cout << intro << ++nlab << "\tp (K+) GJ {pkpgj}" << endl;

		cout << intro << ++nlab << "\tcos(theta) GJ (K+) {ctxgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI GJ (K+) {phixgj}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}

	if (print || (strcmp(c, "M6") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M6 (K+ K- pi+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/K+ K- pi+ {mm2kkpi}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K-) {mkk}" << endl;
		cout << intro << ++nlab << "\tMass(K- pi+) {mKmpip}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K- pi+) {mKKpi}" << endl;
		cout << intro << ++nlab << "\tMass(K+ X) {mKpX}" << endl;
		cout << intro << ++nlab << "\tMass/(K- X) {mKmX}" << endl;
		cout << intro << ++nlab << "\tMass/(pi+ X) {mpipX}" << endl;

		cout << intro << ++nlab << "\tMass (K+ n) neutron constrained {mKpn}"
				<< endl;

		cout << intro << ++nlab << "\t-t {t}" << endl;
		cout << intro << ++nlab << "\tmissing pt {mpt}" << endl;
		cout << intro << ++nlab << "\tmissing pz {mpz}" << endl;
		
		printTrackLabels(&nlab,"Kp","kp");
		printTrackLabels(&nlab,"Km","km");
		printTrackLabels(&nlab,"PiP","pip");
		
		cout << intro << ++nlab << "\tcos(theta) CM K- pi+ {ctKmpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM K- pi+ {phiKmpip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM pi+ {ctpipx}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM K- pi+ {phipipx}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM K+ {ctKp}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM K+ {phiKp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM K- {ctKm}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM K- {phiKm}" << endl;

		cout << intro << ++nlab << "\tcos(theta) hel pi+ {chelpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI hel pi+ {phihelpip}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}

	if (print || (strcmp(c, "M51") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M51 (K+ pi- pi+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/K+ pi- pi+ {mm2kpipi}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ Ks {mm2kpks}" << endl;
		cout << intro << ++nlab << "\tMM^2/pi+ pi- {mm2pippim}" << endl;
		cout << intro << ++nlab << "\tMM^2/Ks {mm2Ks}" << endl;
		cout << intro << ++nlab << "\tMass(K+ pi-) {mkppim}" << endl;
		cout << intro << ++nlab << "\tMass(pi- pi+) {mpipi}" << endl;
		cout << intro << ++nlab << "\tMass(K+ pi- pi+) {mKpipi}" << endl;
		cout << intro << ++nlab << "\tMass(K+ n) {mKpn}" << endl;
		cout << intro << ++nlab << "\tMass(K+ X) {mKpX}" << endl;
		cout << intro << ++nlab << "\tMass/(pi- X) {mpimn}" << endl;
		cout << intro << ++nlab << "\tMass/(pi- X) {mpimX}" << endl;
		cout << intro << ++nlab << "\tMass/(pi+ X) {mpipn}" << endl;
		cout << intro << ++nlab << "\tMass/(pi+ X) {mpipX}" << endl;
		cout << intro << ++nlab << "\tMass Ks n {mKsn}" << endl;
		cout << intro << ++nlab << "\tMass Ks Kp {mKsKp}" << endl;
		cout << intro << ++nlab << "\t-t (n) {tn}" << endl;
		cout << intro << ++nlab << "\t-t (X) {tX}" << endl;
		cout << intro << ++nlab << "\t-u (beam -> Ks) {u}" << endl;
		cout << intro << ++nlab << "\tmissing pt {mpt}" << endl;
		cout << intro << ++nlab << "\tmissing pz {mpz}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM pi pi {ctpipi}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM pi pi {phipipi}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM Ks {ctKs}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM Ks {phiKs}" << endl;
		cout << intro << ++nlab << "\tcos(theta) hel pi+ {chelpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI hel pi+ {phihelpip}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}

	if (print || (strcmp(c, "M7") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M7 (p pi+ pi0):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p pi+ pi0 {mm2ppippi0}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi0) {mpippi0}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi0) {mppi0}" << endl;
		cout << intro << ++nlab << "\tMass(p pi0 pi+) {mpippi0}" << endl;
		cout << intro << ++nlab << "\tMM^2/(p pi+) {mm2ppip}" << endl;
		cout << intro << ++nlab << "\tMM^2/(p pi0) {mm2ppi0}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "M8") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M8 (p K+ pi0):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p K+ pi0 {mm2ppippim}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMass(K+ pi0) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(p K+) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi0) {mppim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi0 K+) {mpippim}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
	}

	if (print || (strcmp(c, "M9") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M9 (p pi+ pi- pi0):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p pi+ pi- pi0 {mm2ppippimpi0}"
				<< endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi- pi0) {mpippimpi0}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi0) {mpippi0}" << endl;
		cout << intro << ++nlab << "\tMass(pi0 pi-) {mpi0pim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi-) {mppim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi0) {mppi0}" << endl;
		cout << intro << ++nlab << "\tMass(p pi- pi+ pi0) {mpippimpi0}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}

	if (print || (strcmp(c, "M10") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M10 (gamma gamma ...):" << endl;
		printGeneralHeaderLabels(&nlab);
		printPhotonLabels(nlab);
		ret = 1;
	}
	if (print || (strcmp(c, "M11") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M11 (proton proton...):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tmm2/pp {mm2pp}" << endl;
		cout << intro << ++nlab << "\tmm2p1 {mm2p1}" << endl;
		cout << intro << ++nlab << "\tmm2p2 {mm2p2}" << endl;
		cout << intro << ++nlab << "\tmissing px {mpx}" << endl;
		cout << intro << ++nlab << "\tmissing py {mpy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {mpz}" << endl;
		cout << intro << ++nlab << "\tmissing p {mp}" << endl;

		cout << ++nlab << "\tp1 x {p1x}" << endl;
		cout << ++nlab << "\tp1 y {p1y}" << endl;
		cout << ++nlab << "\tp1 z {p1z}" << endl;
		cout << ++nlab << "\tp1 t {p1t}" << endl;
		cout << ++nlab << "\tp2 x {p2x}" << endl;
		cout << ++nlab << "\tp2 y {p2y}" << endl;
		cout << ++nlab << "\tp2 z {p2z}" << endl;
		cout << ++nlab << "\tp2 t {p2t}" << endl;

		cout << intro << ++nlab << "\tt: beam to p1 and miss {t1}" << endl;
		cout << intro << ++nlab << "\tt: beam to p2 and miss {t2}" << endl;
		cout << intro << ++nlab << "\tcos theta lab between p1 and p2 {cthpp}"
				<< endl;

		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
	}

	if (print || (strcmp(c, "M12") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M12 (pi+ pi0):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\t# or protons {nprot}" << endl;
		cout << intro << ++nlab << "\t# or pi- {npim}" << endl;
		cout << intro << ++nlab << "\tMM^2/pi+ pi0 {mm2pippi0}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi0) {mpippi0}" << endl;
		cout << intro << ++nlab << "\tMass(X pi+) {mxpip}" << endl;
		cout << intro << ++nlab << "\tMass(X pi0) {mxpi0}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		ret = 1;
	}
	if (print || (strcmp(c, "M16") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M12 (pi+ pi-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/pi+ pi- {mm2pippim}" << endl;
		cout << intro << ++nlab << "\t-t/pi+ pi- {t}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(X pi+) {mxpip}" << endl;
		cout << intro << ++nlab << "\tMass(X pi-) {mxpim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi-) {mppim}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tphi/PI of proton in lab{phiplab}" << endl;
		printTrackLabels(&nlab, "pi+", "pip");
		printTrackLabels(&nlab, "pi-", "pim");
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		ret = 1;
	}

	if (print || (strcmp(c, "M13") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M13 (p):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) p  CM {ctcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI p CM {phicm}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
	}
	if (print || (strcmp(c, "M14") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M14 (pi+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p+ {mm2pip}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) pi+  CM {ctcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI pi+ CM {phicm}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
	}
	if (print || (strcmp(c, "M15") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M15 (proton + pi-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/(p pi-) {mm2ppim}" << endl;
		cout << intro << ++nlab << "\tMass(p pi-) {mppim}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab p {ctlabp}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab p {philabp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab pi- {ctlabpim}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab pi- {philabpim}" << endl;
		cout << intro << ++nlab << "\tp (proton) Lab  {pp}" << endl;
		cout << intro << ++nlab << "\tp (pi-) Lab {ppim}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM pi- {ctcmpim}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM pi- {phicmpim}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM X {ctcmx}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM X {phicmx}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J pi- {ctgjpim}" << endl;
		cout << intro << ++nlab << "\tphi/PI G-J pi- {phigjpim}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "M20") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M20 (n pi+ pi+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/n pi+ pi+ {mm2npippip}" << endl;
		cout << intro << ++nlab << "\tMM^2/n {mm2p}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi+) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(n pi+1) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(n pi+2) {mppim}" << endl;
		cout << intro << ++nlab << "\tMass(n pi+ pi+) {mppippim}" << endl;
		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		printPhotonLabels(nlab);
		ret = 1;

	}

	if (print || (strcmp(c, "M24") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M24 (K+ K-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\t# of protons {nprot}" << endl;
		cout << intro << ++nlab << "\tMM^2/(K+ K-) {mm2KK}" << endl;
		cout << intro << ++nlab << "\tMM^2/(K+) {mm2Kp}" << endl;
		cout << intro << ++nlab << "\tMM^2/(K-) {mm2Km}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K-) {mkk}" << endl;
		cout << intro << ++nlab << "\tMissing pt {misspt}" << endl;
		cout << intro << ++nlab << "\tMissing pz {misspz}" << endl;
		cout << intro << ++nlab << "\tcos theta K+ (lab) {ctKp}" << endl;
		cout << intro << ++nlab << "\tphi/PI K+ (lab_ {phiKp}" << endl;
		cout << intro << ++nlab << "\tp K+ (lab) {pKp}" << endl;

		cout << intro << ++nlab << "\tcos theta K- (lab) {ctKm}" << endl;
		cout << intro << ++nlab << "\tphi/PI K- (lab_ {phiKm}" << endl;
		cout << intro << ++nlab << "\tp K- (lab) {pKm}" << endl;
		
		cout << intro;
		printTrackLabels(&nlab,"Kp","kp");
		cout << intro;
		printTrackLabels(&nlab,"Km","km");
		

		cout << intro << ++nlab << "\t-t {t}" << endl;
		cout << intro << ++nlab << "\t-t to K+ {tkp}" << endl;
		cout << intro << ++nlab << "\t-t to K- {tkm}" << endl;

		cout << intro << ++nlab << "\tm(K+ X) {mKpX}" << endl;
		cout << intro << ++nlab << "\tm(K- X) {mKmX}" << endl;
		cout << intro << ++nlab << "\ttof mass of K+ {mkp}" << endl;
		cout << intro << ++nlab << "\ttof mass of K- {mkm}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ct}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phi}" << endl;
		cout << intro << ++nlab << "\tp in KK system  {pkk}" << endl;
		cout << intro << ++nlab << "\tcos(theta) GJ (K+) {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI GJ (K+) {phigj}" << endl;
		ret = 1;

	}

	if (print || (strcmp(c, "M28") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M28 (K+ K+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/(K+ K+) {mm2KK}" << endl;
		cout << intro << ++nlab << "\tmissing transverse momentum {misspt}"
				<< endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tm(X K+1) {mXKp1}" << endl;
		cout << intro << ++nlab << "\tm(X K+2) {mXKp2}" << endl;
		cout << intro << ++nlab << "\tt to K+1 {t1}" << endl;
		cout << intro << ++nlab << "\tt to K+2 {t2}" << endl;
		cout << intro << ++nlab << "\tp of K+1 {Kp1p}" << endl;
		cout << intro << ++nlab << "\tbeta of K+1 {Kp1b}" << endl;
		cout << intro << ++nlab << "\tp of K+2 {Kp2p}" << endl;
		cout << intro << ++nlab << "\tbeta of K+2 {Kp2b}" << endl;
		cout << intro << ++nlab << "\tmass of K+1 {mKp1}" << endl;
		cout << intro << ++nlab << "\tmass of K+2 {mKp2}" << endl;

		cout << intro;
		printTrackLabels(&nlab,"Kp1","kp1");
		cout << intro;
		printTrackLabels(&nlab,"Kp2","kp2");

		ret = 1;

	}

	if (print || (strcmp(c, "M29") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M29 (proton + pi+ + gamma):" << endl;
		printGeneralHeaderLabels(&nlab);

		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/(p pi+ gam) {mm2ppipgam}" << endl;
		cout << intro << ++nlab << "\tmm^2/p pi+ {mm2ppip}" << endl;
		cout << intro << ++nlab << "\tmm^2/p gam {mm2pgam}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab p {ctlabp}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab p {philabp}" << endl;
		cout << intro << ++nlab << "\tcos(theta) Lab pi+ {ctlabpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI Lab pi+ {philabpip}" << endl;
		cout << intro << ++nlab << "\tp (proton) Lab  {pp}" << endl;
		cout << intro << ++nlab << "\tp (pi+) Lab {ppip}" << endl;

		cout << intro << ++nlab << "\tcos(theta) CM pi+ {ctcmpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM pi+ {phicmpip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM X {ctcmx}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM X {phicmx}" << endl;
		cout << intro << ++nlab << "\tcos(theta) G-J pi+ {ctgjpip}" << endl;
		cout << intro << ++nlab << "\tphi/PI G-J pi+ {phigjpip}" << endl;
		cout << intro << ++nlab << "\tcos(theta) helicity  pi+ {cthpip}"
				<< endl;
		cout << intro << ++nlab << "\tphi/PI helicity pi+ {phihpip}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "M30") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M30 (proton + pi+ + pi- + gamma):" << endl;
		printGeneralHeaderLabels(&nlab);

		cout << intro << ++nlab << "\t-t {t}" << endl;
		cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM^2/(p pi+ pi- gam) {mm2}" << endl;
		cout << intro << ++nlab << "\tmissing momentum {missp}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing transverse momentum {misspt}"
				<< endl;
		cout << intro << ++nlab << "\tmm^2/p pi+ {mm2ppip}" << endl;
		cout << intro << ++nlab << "\tmm^2/p pip gam {mm2ppipgam}" << endl;
		cout << intro << ++nlab << "\tmm^2/p gam {mm2pgam}" << endl;
		cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
		cout << intro << ++nlab << "\tMass(p pi-) {mppim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi- gam) {mpippimgam}" << endl;
		cout << intro << ++nlab << "\tMass(p gam) {mpgam}" << endl;
		cout << intro << ++nlab << "\tE of gamma {egam}" << endl;

		ret = 1;

	}

	if (print || (strcmp(c, "M31") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M31 (pi+ + pi- + gamma):" << endl;
		printGeneralHeaderLabels(&nlab);

		cout << intro << ++nlab << "\t-t (to pi+ pi- gam) {t}" << endl;
		cout << intro << ++nlab << "\tmissing mass {mm}" << endl;
		cout << intro << ++nlab << "\tmissing momentum {missp}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing transverse momentum {misspt}"
				<< endl;
		cout << intro << ++nlab << "\tmm^2/pi+ {mm2pip}" << endl;
		cout << intro << ++nlab << "\tmm^2/pi- {mm2pim}" << endl;
		cout << intro << ++nlab << "\tmm^2/gam {mm2gam}" << endl;
		cout << intro << ++nlab << "\tmm^2/pip gam {mm2pipgam}" << endl;
		cout << intro << ++nlab << "\tmm^2/pim gam {mm2pimgam}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi- gam) {mpippimgam}" << endl;
		cout << intro << ++nlab << "\tE of gamma {egam}" << endl;

		ret = 1;

	}

	if (print || (strcmp(c, "M32") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M32 (pi+ + pi+ + pi- + gamma):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tmX2 {mX2}" << endl;
		cout << intro << ++nlab << "\tMM^2 {mm2}" << endl;
		cout << intro << ++nlab << "\tmissing momentum {missp}" << endl;
		cout << intro << ++nlab << "\tmissing transverse momentum {misspt}"
				<< endl;
		cout << intro << ++nlab << "\tm pi+1 pi- {mpip1pim}" << endl;
		cout << intro << ++nlab << "\tm pi+2 pi- {mpip2pim}" << endl;
		cout << intro << ++nlab << "\tMass(pi+1 pi- gam) {mpip1pimgam}" << endl;
		cout << intro << ++nlab << "\tMass(pi+2 pi- gam) {mpip2pimgam}" << endl;
		cout << intro << ++nlab << "\tMass(pi+ pi+ pi- gam) {mpipipigam}"
				<< endl;

		ret = 1;

	}
	if (print || (strcmp(c, "M38") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M38 (e+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tE (e+) {E}" << endl;
		cout << intro << ++nlab << "\tp (e+) {p}" << endl;
		cout << intro << ++nlab << "\tcos theta (e+) {ct}" << endl;
		cout << intro << ++nlab << "\tphi/Pi (e+) {phi}" << endl;
		cout << intro << ++nlab << "\tE calorimeter (e+) {Ep}" << endl;
		cout << intro << ++nlab << "\ttime ec (e+) {tecp}" << endl;
	}
	if (print || (strcmp(c, "M39") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M38 (e+):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tE (e-) {E}" << endl;
		cout << intro << ++nlab << "\tp (e-) {p}" << endl;
		cout << intro << ++nlab << "\tcos theta (e-) {ct}" << endl;
		cout << intro << ++nlab << "\tphi/Pi (e-) {phi}" << endl;
		cout << intro << ++nlab << "\tE calorimeter (e-) {Ep}" << endl;
		cout << intro << ++nlab << "\ttime ec (e-) {tecm}" << endl;

	}
	if (print || (strcmp(c, "M35") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M35 (e+ e-):" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tMM^2/e+ e- {mm2epem}" << endl;
		cout << intro << ++nlab << "\tt {t}" << endl;
		cout << intro << ++nlab << "\tt' {tp}" << endl;
		cout << intro << ++nlab << "\tp e+ {pep}" << endl;
		cout << intro << ++nlab << "\tcos theta e+ {ctep}" << endl;
		cout << intro << ++nlab << "\tphi/Pi e+ {phiep}" << endl;
		cout << intro << ++nlab << "\te+ TOF vertex time {scvtep}" << endl;
		cout << intro << ++nlab << "\tpi+ TOF vertex time {scvtpip}" << endl;
		cout << intro << ++nlab << "\te+ TOF pathlen {pathep}" << endl;
		cout << intro << ++nlab << "\tp e- {pem}" << endl;
		cout << intro << ++nlab << "\tcos theta e- {ctem}" << endl;
		cout << intro << ++nlab << "\tphi/Pi e- {phiem}" << endl;
		cout << intro << ++nlab << "\te- TOF vertex time {scvtem}" << endl;
		cout << intro << ++nlab << "\tpi- TOF vertex time {scvtpim}" << endl;
		cout << intro << ++nlab << "\te- TOF pathlen {pathem}" << endl;
		cout << intro << ++nlab << "\tMass(e+ e-) {mepem}" << endl;
		cout << intro << ++nlab << "\tp(e+e-) {pepem}" << endl;
		cout << intro << ++nlab << "\tbeta(e+e-) {beta}" << endl;
		cout << intro << ++nlab << "\tMass(X e+) {mxep}" << endl;
		cout << intro << ++nlab << "\tMass(X e-) {mxem}" << endl;
		cout << intro << ++nlab << "\tcos theta lab e+ e- {ct}" << endl;

		cout << intro << ++nlab << "\tmissing px {missx}" << endl;
		cout << intro << ++nlab << "\tmissing py {missy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
		cout << intro << ++nlab << "\tmissing p {missp}" << endl;
		cout << intro << ++nlab << "\tphi/PI of proton in lab{phiplab}" << endl;
		cout << intro << ++nlab << "\tcos(theta) CM {ctgj}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phigj}" << endl;
		ret = 1;
	}
	
	if (print || (strcmp(c, "M54") == 0)) {
			int nlab = 0;
			cout << intro << "Labels for M54 (p pi+ pi- gam gam):" << endl;
			printGeneralHeaderLabels(&nlab);
			cout << intro << ++nlab << "\t# gam{ngam}" << endl;
			cout << intro << ++nlab << "\tMM^2/p pi+ pi- gam gam {mm2}" << endl;
			cout << intro << ++nlab << "\tMM^2/p pi+ pi- {mm2ppippim}" << endl;
			cout << intro << ++nlab << "\tMM^2/p {mm2p}" << endl;
			cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
			cout << intro << ++nlab << "\tMass(gam gam) {mgg}" << endl;
			cout << intro << ++nlab << "\tMass(pi+ pi- gam gam) {mpipigg}" << endl;
			cout << intro << ++nlab << "\tMass(p pi+) {mppip}" << endl;
			cout << intro << ++nlab << "\tMass(p pi-) {mppim}" << endl;
			cout << intro << ++nlab << "\tMass(p pi- pi+) {mppippim}" << endl;
			cout << intro << ++nlab << "\tt {t}" << endl;
			cout << intro << ++nlab << "\tt' {tprime}" << endl;
			cout << intro << ++nlab << "\tmissing px {missx}" << endl;
			cout << intro << ++nlab << "\tmissing py {missy}" << endl;
			cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
			cout << intro << ++nlab << "\tmissing p {missp}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM pi pi {ct}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM pi pi {phi}" << endl;
			cout << intro << ++nlab << "\tcos(theta) GJ pi+ {ctgj}" << endl;
			cout << intro << ++nlab << "\tphi/PI GJ pi+ {phigj}" << endl;
			cout << intro << ++nlab << "\tcos(theta) helicity pi+ {cth}" << endl;
			cout << intro << ++nlab << "\tphi/PI helicity pi+ {phih}" << endl;
			ret = 1;

		}
	
	if (print || (strcmp(c, "M55") == 0)) {
			int nlab = 0;
			cout << intro << "Labels for M55 (K+ K+ pi-):" << endl;
			printGeneralHeaderLabels(&nlab);
			cout << intro << ++nlab << "\tMM^2/K+ K+ pi- {mm2kkpi}" << endl;
			cout << intro << ++nlab << "\tMass(K+ K+) {mkk}" << endl;
			cout << intro << ++nlab << "\tMass(K+1 pi-) {mKp1pim}" << endl;
			cout << intro << ++nlab << "\tMass(K+2 pi-) {mKp2pim}" << endl;

			cout << intro << ++nlab << "\tMass(K+ K+ pi-) {mKKpi}" << endl;
			cout << intro << ++nlab << "\tMass(K+1 X) {mKp1X}" << endl;
			cout << intro << ++nlab << "\tMass(K+2 X) {mKp2X}" << endl;

			cout << intro << ++nlab << "\tMass/(pi- X) {mpimX}" << endl;

			cout << intro << ++nlab << "\t-t {t}" << endl;
			cout << intro << ++nlab << "\tmissing pt {mpt}" << endl;
			cout << intro << ++nlab << "\tmissing pz {mpz}" << endl;
			
			printTrackLabels(&nlab,"Kp1","kp1");
			printTrackLabels(&nlab,"Kp2","kp2");
			printTrackLabels(&nlab,"PiM","pim");
			
			cout << intro << ++nlab << "\tcos(theta) CM K+1 pi- {ctKp1pim}" << endl;
			

			cout << intro << ++nlab << "\tphi/PI CM K+1 pi- {phiKp1pim}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM K+2 pi- {ctKp2pim}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM K+2 pi- {phiKp2pim}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM pi- {ctpimx}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM pi- {phipimx}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM K+1 {ctKp1}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM K+1 {phiKp1}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM K+2 {ctKp2}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM K+2 {phiKp2}" << endl;
			
			cout << intro << ++nlab << "\tcos(theta) hel pi- {chelpim}" << endl;
			cout << intro << ++nlab << "\tphi/PI hel pi- {phihelpim}" << endl;
			printPhotonLabels(nlab);
			ret = 1;

		}
	if (print || (strcmp(c, "M57") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for M57 (p K+ K- Pi+ Pi-):" << endl;
		printGeneralHeaderLabels(&nlab);
		
		//MM^2 3 combinatorial, 5 choose 3
		cout << intro << ++nlab << "\tMM^2/p K+ K- {mm2pkk}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K+ Pi+ {mm2pkppp}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K+ Pi- {mm2pkppm}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K- Pi+ {mm2pkmpp}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K- Pi- {mm2pkmpm}" << endl;
		cout << intro << ++nlab << "\tMM^2/p Pi+ Pi- {mm2ppppm}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ K- Pi+ {mm2kpkmpp}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ K- Pi- {mm2kpkmpm}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ Pi+ Pi- {mm2kppppm}" << endl;
		cout << intro << ++nlab << "\tMM^2/K- Pi+ Pi- {mm2kmkmpm}" << endl;
		
		//Mass 3 combinatorial
		cout << intro << ++nlab << "\tMass(p K+ K-) {mpkk}" << endl;
		cout << intro << ++nlab << "\tMass(p K+ Pi+) {mpkppp}" << endl;
		cout << intro << ++nlab << "\tMass(p K+ Pi-) {mpkppm}" << endl;
		cout << intro << ++nlab << "\tMass(p K- Pi+) {mpkmpp}" << endl;
		cout << intro << ++nlab << "\tMass(p K- Pi-) {mpkmpm}" << endl;
		cout << intro << ++nlab << "\tMass(p Pi+ Pi-) {mppppm}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K- Pi+) {mkpkmpp}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K- Pi-) {mkpkmpm}" << endl;
		cout << intro << ++nlab << "\tMass(K+ Pi+ Pi-) {mkppppm}" << endl;
		cout << intro << ++nlab << "\tMass(K- Pi+ Pi-) {mkmkmpm}" << endl;
		
		//MM^2 2 combinatorial 
		cout << intro << ++nlab << "\tMM^2/p K+ {mm2pkp}" << endl;
		cout << intro << ++nlab << "\tMM^2/p K- {mm2pkm}" << endl;
		cout << intro << ++nlab << "\tMM^2/p Pi+ {mm2ppp}" << endl;
		cout << intro << ++nlab << "\tMM^2/p Pi- {mm2ppm}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ K- {mm2kpkm}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ Pi+ {mm2kppp}" << endl;
		cout << intro << ++nlab << "\tMM^2/K+ Pi- {mm2kppm}" << endl;
		cout << intro << ++nlab << "\tMM^2/K- Pi+ {mm2kmpp}" << endl;
		cout << intro << ++nlab << "\tMM^2/K- Pi- {mm2kmpm}" << endl;
		cout << intro << ++nlab << "\tMM^2/Pi+ Pi- {mm2pppm}" << endl;
		
		//Mass 2 combinatorial 
		cout << intro << ++nlab << "\tMass(p K+) {mpkp}" << endl;
		cout << intro << ++nlab << "\tMass(p K-) {mpkm}" << endl;
		cout << intro << ++nlab << "\tMass(p Pi+) {mppp}" << endl;
		cout << intro << ++nlab << "\tMass(p Pi-) {mppm}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K-) {mkpkm}" << endl;
		cout << intro << ++nlab << "\tMass(K+ Pi+) {mkppp}" << endl;
		cout << intro << ++nlab << "\tMass(K+ Pi-) {mkppm}" << endl;
		cout << intro << ++nlab << "\tMass(K- Pi+) {mkmpp}" << endl;
		cout << intro << ++nlab << "\tMass(K- Pi-) {mkmpm}" << endl;
		cout << intro << ++nlab << "\tMass(Pi+ Pi-) {mpppm}" << endl;
		
		
		cout << intro << ++nlab << "\tMM^2/p K+ K- Pi+ Pi- {mm2pkkpp}" << endl;
		cout << intro << ++nlab << "\tMass(p K- K+ Pi+ Pi-) {mpkkpp}" << endl;
		
		cout << intro << ++nlab << "\tMM^2/K+ K- Pi+ Pi- {mm2kkpp}" << endl;
		cout << intro << ++nlab << "\tMass(K+ K- Pi+ Pi-) {mkkpp}" << endl;
		
		
		cout << intro << ++nlab << "\tMM2/(p) {mm2p}" << endl;
		cout << intro << ++nlab << "\tMM2/(K+) {mm2kp}" << endl;
		cout << intro << ++nlab << "\tMM2/(K-) {mm2km}" << endl;
		cout << intro << ++nlab << "\tMM2/(K+) {mm2pp}" << endl;
		cout << intro << ++nlab << "\tMM2/(K-) {mm2pm}" << endl;
		
		cout << intro << ++nlab << "\tmissing px {mpx}" << endl;
		cout << intro << ++nlab << "\tmissing py {mpy}" << endl;
		cout << intro << ++nlab << "\tmissing pz {mpz}" << endl;
		cout << intro << ++nlab << "\tmissing p {mp}" << endl;
		
		cout << intro << ++nlab << "\tp beta {pb}" << endl;
		cout << intro << ++nlab << "\tp p {pp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab p {ctp}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab p {phip}" << endl;
		
		cout << intro << ++nlab << "\tK+ beta {kpb}" << endl;
		cout << intro << ++nlab << "\tK+  p {kpp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab K+ {ctkp}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab K+ {phikp}" << endl;
		
		cout << intro << ++nlab << "\tK- beta {kmb}" << endl;
		cout << intro << ++nlab << "\tK-  p {kmp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab K- {ctkm}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab K- {phikm}" << endl;
		
		cout << intro << ++nlab << "\tPi+ beta {ppb}" << endl;
		cout << intro << ++nlab << "\tPi+  p {ppp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab Pi+ {ctpp}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab Pi+ {phipp}" << endl;
		
		cout << intro << ++nlab << "\tPi- beta {pmb}" << endl;
		cout << intro << ++nlab << "\tPi-  p {pmp}" << endl;
		cout << intro << ++nlab << "\tcos theta lab Pi- {ctpm}" << endl;
		cout << intro << ++nlab << "\tphi/Pi  lab Pi- {phipm}" << endl;
		
		cout << intro;
		printTrackLabels(&nlab,"Kp","kp");
		cout << intro;
		printTrackLabels(&nlab,"Km","km");
		cout << intro;
		printTrackLabels(&nlab,"P","p");
		cout << intro;
		printTrackLabels(&nlab,"Pip","pp");
		cout << intro;
		printTrackLabels(&nlab,"Pim","pm");
		
		
		cout << intro << ++nlab << "\tt to K+ K- {t}" << endl;
		cout << intro << ++nlab << "\tt' to K+ K- {tp}" << endl;
		
		cout << intro << ++nlab << "\tt to K+  {tkp}" << endl;
		cout << intro << ++nlab << "\tt' to K+  {tpkp}" << endl;
		
		cout << intro << ++nlab << "\tt to K-  {tkm}" << endl;
		cout << intro << ++nlab << "\tt' to K-  {tpkm}" << endl;
		
		cout << intro << ++nlab << "\tt to K+ K- X {tkpkmX}" << endl;
		cout << intro << ++nlab << "\tt' to K+ K- X  {tpkpkmX}" << endl;
		
		cout << intro << ++nlab << "\tt to K+ K- Pi+ Pi- X {tkpkmpppmX}" << endl;
		cout << intro << ++nlab << "\tt' to K+ K- Pi+ Pi- X  {tpkpkmpppmX}" << endl;
		
		cout << intro << ++nlab << "\tdelta beta K+ {dbKp}" << endl;
		cout << intro << ++nlab << "\tdelta beta K- {dbKm}" << endl;
		cout << intro << ++nlab << "\tdelta beta proton {dbp}" << endl;
		cout << intro << ++nlab << "\tdelta K+ time  {dtKp}" << endl;
		cout << intro << ++nlab << "\tdelta K- time  {dtKm}" << endl;
		cout << intro << ++nlab << "\tdelta proton time {dtp}" << endl;
		cout << intro << ++nlab << "\tmissing pt {m_pt}" << endl;
		cout << intro << ++nlab << "\tmissing pz {m_pz}" << endl;
		
		cout << intro << ++nlab << "\tcos(theta) CM {ct}" << endl;
		cout << intro << ++nlab << "\tphi/PI CM {phi}" << endl;
		
		cout << intro << ++nlab << "\tp of p CM {ppcm}" << endl;
		cout << intro << ++nlab << "\tcos theta p CM {ctpcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI p CM {phipcm}" << endl;
		
		cout << intro << ++nlab << "\tp of K+ CM {pKpcm}" << endl;
		cout << intro << ++nlab << "\tcos theta K+ CM {ctKpcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI K+ CM {phiKpcm}" << endl;
		
		cout << intro << ++nlab << "\tp of K- CM {pKmcm}" << endl;
		cout << intro << ++nlab << "\tcos theta K- CM {ctKmcm}" << endl;
		cout << intro << ++nlab << "\tphi/PI K- CM {phiKmcm}" << endl;
		
		//cout << intro << ++nlab << "\tp in KK system  {pkk}" << endl;
		
		//cout << intro << ++nlab << "\tcos(theta) GJ (K+K-) {ctgj}" << endl;
		//cout << intro << ++nlab << "\tphi/PI GJ (K+K-) {phigj}" << endl;
		
		//cout << intro << ++nlab << "\tp (K+) GJ {pkpgj}" << endl;
		
		//cout << intro << ++nlab << "\tcos(theta) GJ (K+) {ctxgj}" << endl;
		//cout << intro << ++nlab << "\tphi/PI GJ (K+) {phixgj}" << endl;
		printPhotonLabels(nlab);
		ret = 1;
		
	}
	

	
	
	

	if (print || (strcmp(c, "TAGT") == 0)) {
		int nlab = 0;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\t# T-counter hits" << endl;
		cout << intro << ++nlab << "\tminimum T-counter hit" << endl;
		cout << intro << ++nlab << "\tmaximum T-counter hit" << endl;
		ret = 1;

	}

	if (print || (strcmp(c, "ST") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for ST:" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tST time1" << endl;
		cout << intro << ++nlab << "\tST time2" << endl;
		cout << intro << ++nlab << "\tTAGR t_id" << endl;
		cout << intro << ++nlab << "\tTAGR time" << endl;
		cout << intro << ++nlab << "\tTAGR time RF corrected" << endl;
		cout << intro << ++nlab << "\tTAGR time - mean ST time" << endl;
		cout << intro << ++nlab << "\tTAGR time - mean ST time RF correctded"
				<< endl;
		ret = 1;

	}

	if (print || (strcmp(c, "VERT") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for VERT:" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\t# vertices {nvert}" << endl;
		cout << intro << ++nlab << "\tx {x}" << endl;
		cout << intro << ++nlab << "\ty {y}" << endl;
		cout << intro << ++nlab << "\tz {z}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "MVRT") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for MVRT:" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\t# vertices {nvert}" << endl;
		cout << intro << ++nlab << "\tx {x}" << endl;
		cout << intro << ++nlab << "\ty {y}" << endl;
		cout << intro << ++nlab << "\tz {z}" << endl;
		ret = 1;

	}
	if (print || (strcmp(c, "NEUT") == 0)) {
		int nlab = 0;
		cout << intro << "Labels for NEUT:" << endl;
		printGeneralHeaderLabels(&nlab);
		cout << intro << ++nlab << "\tpid (geant) {pid}" << endl;
		cout << intro << ++nlab << "\tbeta {beta}" << endl;
		cout << intro << ++nlab << "\tmomentum {p}" << endl;
		cout << intro << ++nlab << "\tvertex x {x}" << endl;
		cout << intro << ++nlab << "\tvertex y {y}" << endl;
		cout << intro << ++nlab << "\tvertex z {z}" << endl;
		ret = 1;

	}
	return (ret);
}

int printPhotonLabels(int nlab) {
	char *intro = "";

	if (matlab)
		intro = "%\t";
	cout << intro << ++nlab << "\t# gammas {ngam}" << endl;
	cout << intro << ++nlab << "\t# nneutrons {nneut}" << endl;
	cout << intro << ++nlab << "\tE gamma 1 {Egm1}" << endl;
	cout << intro << ++nlab << "\tE gamma 2 {Egm2}" << endl;
	cout << intro << ++nlab << "\tE gamma 3 {Egm3}" << endl;
	cout << intro << ++nlab << "\tgamma gamma mass 1 {ggm1}" << endl;
	cout << intro << ++nlab << "\tgamma gamma mass 2 {ggm2}" << endl;
	cout << intro << ++nlab << "\tgamma gamma mass 3 {ggm3}" << endl;
	cout << intro << ++nlab << "\tphi/Pi gamma 1 {phi1}" << endl;
	cout << intro << ++nlab << "\tphi/Pi gamma 2 {phi2}" << endl;
	cout << intro << ++nlab << "\tphi/Pi gamma 3 {phi3}" << endl;
	cout << intro << ++nlab << "\tcos theta gamma 1 {cost1}" << endl;
	cout << intro << ++nlab << "\tcos theta gamma 2 {cost2}" << endl;
	cout << intro << ++nlab << "\tcos theta gamma 3 {cost3}" << endl;
	cout << intro << ++nlab << "\tsector gamma 1 {sec1}" << endl;
	cout << intro << ++nlab << "\tsector gamma 2 {sec2}" << endl;
	cout << intro << ++nlab << "\tsector gamma 3 {sec3}" << endl;

	cout << intro << ++nlab << "\tgamma gamma energy 1 {ggE1}" << endl;
	cout << intro << ++nlab << "\tgamma gamma energy 2 {ggE2}" << endl;
	cout << intro << ++nlab << "\tgamma gamma energy 3 {ggE3}" << endl;

	cout << intro << ++nlab << "\tgamma gamma cos theta 1 {ggcost1}" << endl;
	cout << intro << ++nlab << "\tgamma gamma cos theta 2 {ggcost2}" << endl;
	cout << intro << ++nlab << "\tgamma gamma cos theta 3 {ggcost3}" << endl;

	cout << intro << ++nlab << "\tgamma gamma phi/Pi 1 {ggphi1}" << endl;
	cout << intro << ++nlab << "\tgamma gamma phi/Pi 2 {ggphi2}" << endl;
	cout << intro << ++nlab << "\tgamma gamma phi/Pi 3 {ggphi3}" << endl;
	return (nlab);
}

int printLabels(Event_t mode) {
	int ret = 0;
	char *intro = "";

	if (matlab)
		intro = "%\t";
	int nlab = 0;

	char s[50];
	sprintf(s, "M%d", (int) mode);
	if (!printGeneralLabels(s)) {
		switch (mode) {

		case DstKmPipPim:
			cout << intro << "Labels for M" << (int) mode << " "
					<< ModeName(mode) << endl;
			printGeneralHeaderLabels(&nlab);
			cout << intro << ++nlab << "\tMM^2/K- pi+ pi- {mm2Kmpippim}"
					<< endl;
			cout << intro << ++nlab << "\tMM^2/K- {mm2Km}" << endl;
			cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
			cout << intro << ++nlab << "\tMass(K- pi+) {mKmpip}" << endl;
			cout << intro << ++nlab << "\tMass(K- pi-) {mKmpim}" << endl;
			cout << intro << ++nlab << "\tMass(K- pi- pi+) {mKmpippim}" << endl;
			cout << intro << ++nlab << "\tt {t}" << endl;
			cout << intro << ++nlab << "\tmissing px {missx}" << endl;
			cout << intro << ++nlab << "\tmissing py {missy}" << endl;
			cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
			cout << intro << ++nlab << "\tmissing p {missp}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM pi pi {ct}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM pi pi{phi}" << endl;
			cout << intro << ++nlab << "\tcos(theta) GJ pi+ {ctgj}" << endl;
			cout << intro << ++nlab << "\tphi/PI GJ pi+ {phigj}" << endl;
			cout << intro << ++nlab << "\tcos(theta) helicity pi+ {cth}"
					<< endl;
			cout << intro << ++nlab << "\tphi/PI helicity pi+ {phih}" << endl;
			printPhotonLabels(nlab);
			ret = 1;
			break;

		case DstKpPipPim:
			cout << intro << "Labels for M" << (int) mode << " "
					<< ModeName(mode) << endl;
			printGeneralHeaderLabels(&nlab);
			cout << intro << ++nlab << "\tMM^2/K+ pi+ pi- {mm2Kppippim}"
					<< endl;
			cout << intro << ++nlab << "\tMM^2/K+ {mm2Kp}" << endl;
			cout << intro << ++nlab << "\tMass(pi+ pi-) {mpippim}" << endl;
			cout << intro << ++nlab << "\tMass(K+ pi+) {mKppip}" << endl;
			cout << intro << ++nlab << "\tMass(K+ pi-) {mKppim}" << endl;
			cout << intro << ++nlab << "\tMass(K+ pi- pi+) {mKppippim}" << endl;
			cout << intro << ++nlab << "\tt {t}" << endl;
			cout << intro << ++nlab << "\tmissing px {missx}" << endl;
			cout << intro << ++nlab << "\tmissing py {missy}" << endl;
			cout << intro << ++nlab << "\tmissing pz {missz}" << endl;
			cout << intro << ++nlab << "\tmissing p {missp}" << endl;
			cout << intro << ++nlab << "\tcos(theta) CM pi pi {ct}" << endl;
			cout << intro << ++nlab << "\tphi/PI CM pi pi{phi}" << endl;
			cout << intro << ++nlab << "\tcos(theta) GJ pi+ {ctgj}" << endl;
			cout << intro << ++nlab << "\tphi/PI GJ pi+ {phigj}" << endl;
			cout << intro << ++nlab << "\tcos(theta) helicity pi+ {cth}"
					<< endl;
			cout << intro << ++nlab << "\tphi/PI helicity pi+ {phih}" << endl;
			printPhotonLabels(nlab);
			ret = 1;
			break;
		default:
			break;

		}
	}

	else {
		ret = 1;
	}
	return (ret);

}

void pHeader(clasEvent &evt) {
	clasHEAD_t *HEAD = (clasHEAD_t *)getBank(&bcs_, "HEAD");
	if (HEAD) {
		cout << evt.run() << " " << evt.event() << " " << evt.trig() << " "
				<< modes(evt) << " ";
	} else
		cout << " -1 -1 0 0 ";

}

int Statistics(int x, clasEvent &evt) {
	int last = (int) DstAll + 1;
	int n = 0;
	static int ntot = 0;
	static int nModes[NMODES] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	if (evt.type() == 1 || evt.type() == MONTE_CARLO_TYPE || evt.type() == -4) {
		if (!x) { // count
			ntot++;
			for (int i = 0; i < last; ++i) {
				if (isEvent( (Event_t) i, evt)) {
					nModes[i]++;
					n++;
				}
			}

		}
	}
	if (x == 1) {
		//    cerr << "MODES: " << NMODES << "last: " << last << endl;
		cerr << "\n\nStatistics:\t" << ntot << endl;
		for (int i = 0; i < last; ++i) {
			float frac = ntot ? (float) nModes[i]/(float) ntot : 0.0;
			cerr << "\t" << i << "\t" << ModeName((Event_t) i) << "\t"
					<< nModes[i] << "\t" << frac << endl;
		}
		cerr << "\n";
		//   printModes();
	}

	return (1);
}
int triggerStatistics(int x, clasEvent &evt) {
	static int ntot0 = 0;
	static int ntrig0[32] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	static int ntot1 = 0;
	static int ntrig1[32] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	if (x == 0) {
		ntot0++;
		for (int i = 0; i < 32; ++i) {
			if (evt.trigBits() & (1 << i)) {
				ntrig0[i]++;
			}
		}
	} else if (x == 1) {
		ntot1++;
		for (int i = 0; i < 32; ++i) {
			if (evt.trigBits() & (1 << i)) {
				ntrig1[i]++;
			}
		}
	} else if (x == 2) {
		cerr << "Trigger statistics:\t" << ntot0 << "\t" << ntot1 << endl;
		for (int i = 0; i < 32; ++i) {
			double percent0 = (double) ntrig0[i]/(double) ntot0 * 100.0;
			double percent1 = (double) ntrig1[i]/(double) ntot1 * 100.0;
			cerr << "\t" << dec << "\t\t" << i + 1 << "\t" << ntrig0[i] << "\t"
					<< ntrig1[i] << "\t" << percent0 << "%\t" << percent1
					<< "%\t" << hex << (1 << i) << dec << endl;
		}
	}
	return (x);
}

int ConfigGeom(int runno) {
	static int CurrentRun=-1;
	int sec;

	if (runno!=CurrentRun) {
		dropAllBanks(&wcs_, "SCG SCP ");
		make_SCG_banks(runno);

		for (sec=1; sec <= 6; sec++) {
			/*clasSCG_t *SCG=NULL;
			 if (SCG=getGroup(&wcs_,"SCG ",sec))printSCGbank(stdout,SCG); */
			make_SCP_bank(sec);
		}
		CurrentRun=runno;
	}
	return (1);
}

int DropList(int DropFlag) {

	/* Mask off banks according to DropFlag*/

	if (DropFlag & DROP_RAW)
		bankList(&bcs_, "E-", "R");
	if (DropFlag & DROP_DC0)
		bankList(&bcs_, "E-", "DC0 ");
	if (DropFlag & DROP_DC1)
		bankList(&bcs_, "E-", "DC1 ");
	if (DropFlag & DROP_HBLA)
		bankList(&bcs_, "E-", "HBLA");
	if (DropFlag & DROP_TBLA)
		bankList(&bcs_, "E-", "TBLA");
	if (DropFlag & DROP_HBTB)
		bankList(&bcs_, "E-", "HBTB");
	if (DropFlag & DROP_SC)
		bankList(&bcs_, "E-", SC_BANKS);
	if (DropFlag & DROP_EC)
		bankList(&bcs_, "E-", EC_BANKS);
	if (DropFlag & DROP_HBID)
		bankList(&bcs_, "E-", "HBID");
	if (DropFlag & DROP_CL01)
		bankList(&bcs_, "E-", "CL01");
	if (DropFlag & DROP_SEB)
		bankList(&bcs_, "E-", SEB_BANKS);
	if (DropFlag & DROP_TBID)
		bankList(&bcs_, "E-", "TBIDPARTTBERTBTR");
	if (DropFlag & DROP_HDPL)
		bankList(&bcs_, "E-", "HDPL");
	if (DropFlag & DROP_TDPL)
		bankList(&bcs_, "E-", "TDPL");
	if (DropFlag & DROP_LAC)
		bankList(&bcs_, "E-", "EC1R");
	if (DropFlag & DROP_CC)
		bankList(&bcs_, "E-", CC_BANKS);
	if (DropFlag & DROP_ST)
		bankList(&bcs_, "E-", ST_BANKS);
	if (DropFlag & DROP_DHCL)
		bankList(&bcs_, "E-", "DHCL");
	if (DropFlag & DROP_TAGR)
		bankList(&bcs_, "E-", TAGGER_BANKS);
	return (1);
}
fourVec echb2partMVRT(echb_t *echb, double mass, threeVec &v, int silentMode) {
	threeVec p, r;
	fourVec pt;
	float energy = gamma_energy(echb);
	float momentum = sqrt(energy * energy - mass * mass);
	r.set(echb->x_hit - v.x(), echb->y_hit - v.y(), echb->z_hit - v.z());

	//  part->p.space.x = energy*(echb->x_hit)/mag;
	//  part->p.space.y = energy*(echb->y_hit)/mag; 
	//  part->p.space.z = energy*(echb->z_hit)/mag;
	//  part->p.t = energy;
	//  part->q = 0;
	p.set(momentum*(r.x()/r.r()), momentum*(r.y()/r.r()), momentum
			*(r.z()/r.r()));
	pt.set(sqrt(p.lenSq() + mass * mass), p);
	return (pt);

}

// -------------------  p pi+ gamma ---------------------

void ProcessPPipGam(Event_t mode, clasEvent &evt, int debug) {

	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, p, gam;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	// X is meson fourVec, miss is missing fourVec

	fourVec X, miss;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	p = evt.cp(Proton,1).p();
	gam = evt.cp(Gamma,1).p();

	nminus = evt.N(-1);
	n0 = evt.N(0);
	nplus = evt.N(1);

	qtot = nplus - nminus;

	// now plot 

	X = beam + target - p;
	miss = beam + target - p - gam - pip;

	cout << "M29 ";
	ProcessGeneralHeader(evt, debug);
	cout << (target - p).lenSq() << " ";
	cout << X.lenSq() << " ";
	cout << miss.lenSq() << " ";
	cout << (beam + target - p - pip).lenSq() << " ";
	cout << (beam + target - p - gam).lenSq() << " ";
	cout << ~(pip + p) << " ";
	// print angle of p and pi+ in lab
	cout << p.cosTheta() << " " << p.phi()/M_PI << " ";
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	// print out momentum of p an pi+ in lab

	cout << p.r() << " " << pip.r() << " ";

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	gam *= L;
	miss *= L;
	X *= L;

	//define the y-axis normal to beam and X
	// target  and the recoil proton

	Cross = beam/X;

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;

	// I am now in the CM frame: print out angles of the pi+, and the meson system

	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";
	cout << X.cosTheta() << " " << X.phi()/M_PI << " ";

	// boost into X rest frame

	L.set(X);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;

	// align beam along z-axis

	L.set(beam.phi(), beam.theta(), 0.0);

	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;

	// write out angles of the pi+
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";

	// Now the helicity frame

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	p = evt.cp(Proton,1).p();
	X = beam + target - p;
	gam *= L;
	miss *= L;

	// Transform to CM system

	L.set(beam + target);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;

	//define the y-axis normal to beam and X
	// target  and the recoil proton

	Cross = beam/X;

	L.set(Cross.phi(), Cross.theta()-M_PI/2.0, -M_PI/2.0);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;

	// I am now in the CM frame- define z axis to be in direction of X  

	L.set(X.phi(), X.theta(), 0.0);

	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;

	// boost into X rest frame

	L.set(X);
	beam *= L;
	target *= L;
	p *= L;
	pip *= L;
	X *= L;
	gam *= L;
	miss *= L;
	// write out angles of the pi+
	cout << pip.cosTheta() << " " << pip.phi()/M_PI << " ";
	cout << endl;
}

// -------------------  p pi+ pi- gamma ---------------------

void ProcessPPipPimGam(Event_t mode, clasEvent &evt, int debug) {

	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pim, p, gam;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	// X is meson fourVec, miss is missing fourVec

	fourVec X, miss;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	pim = evt.cp(PiMinus,1).p();
	p = evt.cp(Proton,1).p();
	gam = evt.cp(Gamma,1).p();

	nminus = evt.N(-1);
	n0 = evt.N(0);
	nplus = evt.N(1);

	qtot = nplus - nminus;

	// now plot 

	X = beam + target - p;
	miss = beam + target - p - gam - pip - pim;

	cout << "M30 ";
	ProcessGeneralHeader(evt, debug);
	cout << -(target - p).lenSq() << " ";
	cout << X.lenSq() << " ";
	cout << miss.lenSq() << " ";
	cout << miss.V().r() << " ";
	cout << miss.V().z() << " ";
	cout << sqrt(miss.V().x()*miss.V().x() + miss.V().x()*miss.V().x()) << " ";
	cout << (beam + target - p - pip).lenSq() << " ";
	cout << (beam + target - p - gam - pip).lenSq() << " ";
	cout << (beam + target - p - gam).lenSq() << " ";
	cout << ~(pip + p) << " ";
	cout << ~(pim + p) << " ";
	cout << ~(pim + pip) << " ";
	cout << ~(pip + pim + gam) << " ";
	cout << ~(p + gam) << " ";
	cout << gam.t() << " ";

	cout << endl;
}

// -------------------  pi+ pi- gamma ---------------------

void ProcessPipPimGam(Event_t mode, clasEvent &evt, int debug) {

	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pip, pim, p, gam;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	// X is meson fourVec, miss is missing fourVec

	fourVec X, miss;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip = evt.cp(PiPlus,1).p();
	pim = evt.cp(PiMinus,1).p();
	gam = evt.cp(Gamma,1).p();

	nminus = evt.N(-1);
	n0 = evt.N(0);
	nplus = evt.N(1);

	qtot = nplus - nminus;

	// now plot 

	X = pip + pim + gam;
	miss = beam + target - X;

	cout << "M31 ";
	ProcessGeneralHeader(evt, debug);
	cout << -(beam - X).lenSq() << " ";
	cout << miss.lenSq() << " ";
	cout << miss.V().r() << " ";
	cout << miss.V().z() << " ";
	cout << sqrt(miss.V().x()*miss.V().x() + miss.V().x()*miss.V().x()) << " ";
	cout << (beam + target - pip).lenSq() << " ";
	cout << (beam + target - pim).lenSq() << " ";
	cout << (beam + target - gam).lenSq() << " ";
	cout << (beam + target - gam - pip).lenSq() << " ";
	cout << (beam + target - gam - pim).lenSq() << " ";

	cout << ~(pim + pip) << " ";
	cout << ~(pip + pim + gam) << " ";
	cout << gam.t() << " ";

	cout << endl;
}

// -------------------  pi+  pi+ pi- gamma ---------------------

void ProcessPipPipPimGam(Event_t mode, clasEvent &evt, int debug) {

	int nplus = 0, nminus = 0, n0 = 0;
	int qtot = 0;
	lorentzTransform L;
	threeVec Cross;
	fourVec pip1, pip2, pim, gam;
	fourVec beam, target(PROTON_MASS,threeVec(0.0,0.0,0.0));

	// X is meson fourVec, miss is missing fourVec

	fourVec X, miss;

	beam = evt.beam().get4P();
	target = evt.target().get4P();
	pip1 = evt.cp(PiPlus,1).p();
	pip2 = evt.cp(PiPlus,2).p();
	pim = evt.cp(PiMinus,1).p();
	gam = evt.cp(Gamma,1).p();

	nminus = evt.N(-1);
	n0 = evt.N(0);
	nplus = evt.N(1);

	qtot = nplus - nminus;

	// now plot 

	X = pip1 + pip2 + gam + pim;
	miss = beam + target - pip1 - gam - pip2 - pim;

	cout << "M32 ";
	ProcessGeneralHeader(evt, debug);
	cout << (beam - X).lenSq() << " ";
	cout << X.lenSq() << " ";
	cout << miss.lenSq() << " ";
	cout << miss.V().r() << " ";
	cout << sqrt(miss.V().x()*miss.V().x() + miss.V().x()*miss.V().x()) << " ";
	cout << ~(pim + pip1) << " ";
	cout << ~(pim + pip2) << " ";
	cout << ~(pip1 + pim + gam) << " ";
	cout << ~(pip2 + pim + gam) << " ";
	cout << ~(pip1 + pip2 + pim + gam) << " ";
	cout << endl;
}

void processTrack(clasParticle &cp) {
	fourVec p = cp.p();
	cout << ~p.V() << " " << p.V().cosTheta() << " " << p.V().phi()/M_PI << " " <<  cp.stVtime() << " ";
	cout << cp.mass() << " " << cp.beta() << " " << cp.Beta() << " ";
	if (cp.isSThit()) {
		cout << " 1 " << cp.SThit().time() << " ";
	
	}
	else {
		cout << " 0 0 ";
	}
	if (cp.isSChit()) {
		cout << " 1 " << cp.SChit().time() << " " << cp.SChit().energy() << " ";
	
	}
	else {
		cout << " 0 0 0 ";
	}
}

void printTrackLabels(int *nlab, char *id1, char *id2) {
	cout << ++(*nlab) << "\tp " << id1 << " {p" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tcos(theta) " << id1 << " {ct" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tphi/PI " << id1 << " {phi" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tstVtime " << id1 << "{stV" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tmass " << id1 << " {m" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tbeta " << id1 << " {b" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tBeta " << id1 << " {B" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tis sthit " << id1 << " {isst" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tst time " << id1 << " {stt" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tis schit " << id1 << " {issc" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tsc time " << id1 << " {sct" << id2 << "}" << endl;
	cout << ++(*nlab) << "\tsc energy " << id1 << " {scE" << id2 << "}" << endl;
	
}

// -----------------------------------------------------

/* Fault handlers */

int installFaultHandlers() {
	signal(SIGINT,signalINT);
	signal(SIGSEGV,signalSEGV);
	//  signal(SIGABRT,signalSEGV);
	//  signal(SIGBUS,signalSEGV); 
	//  signal(SIGPIPE,signalPIPE);
	cerr << "Fault handlers installed" << endl;
	return (1);
}

static void signalINT(int isig) {
	clasEvent e;

	static int count = 0;

	cerr << "Run/Event: " << CurrentRun << " " << CurrentEvent << endl;
	Statistics(1, e);

	count++;
	if (count < 4) {
		signal(isig, signalINT);
	} else {
		exit(0);
	}
}
static void signalSEGV(int isig) {
	clasEvent e;
	static int icount = 0;

	icount++;
	if (icount > 5)
		exit(1);

	cerr << "signalSEGV: Caught signal " << endl;
	cerr << "Run/Event: " << CurrentRun << " " << CurrentEvent << endl;
	//  Statistics(1,e);

}

// debugging

