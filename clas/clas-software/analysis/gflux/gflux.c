/* gflux.c */
/*  $Id: gflux.c,v 1.9 2006/07/17 18:49:05 pasyuk Exp $  */

#ifndef lint
static char vcid[] = "$Id: gflux.c,v 1.9 2006/07/17 18:49:05 pasyuk Exp $";
#endif /* lint */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <call.h>
#include <pid.h>
#include <math.h>
#include <bosddl.h>
#include <clas_cern.h>
#include <map_manager.h>
#include <trip.h>
#include <tripGlobal.h>
#include <tagtnorm.h>
#include <gflux.h>
#include <tagtcl.h>


//-----> declare the bos common blocks

BOSbank bcs_;
BOSbank wcs_;

//-----> default Setting for out of time limits

float beginWindow = 30.;
float runGatedClockFreq = 1.e4;
float endWindow = 160.;
float tdcWindow = 130.;

int NtphoBins = 130;

//-----> counters

int Nevents = 0;
int NnonTrip = 0;
int Ns = 0;

//-----> counters between scaler intervals

int Ncharged = 0; // PART
int Npim = 0;
int Npip = 0;
int Npro = 0;
int Nunknown = 0;

int NchargedG = 0; // GPID
int NpimG = 0;
int NpipG = 0;
int NproG = 0;
int NunknownG = 0;

int NeventsPHY = 0;
int NeventsTRK = 0;
int Nstr = 0;
int Ntagr = 0;

uint32 syncIntervalCumulative = 0;

//-----> flags and switches

int clocklt = 0;
int doExclusive = 0;
int doParticles = 0;
int doRawTagger = 0;
int firsttime;
int max = 0;
int normRun = 0;
int pid = 0;
int startCounter = 0;
int syncCor = 0;
int trip = 0;
int Ecorr = 0;

//-----> variables

float clock;
float runTime;
float runTimeFU;

int fileNo = -1;
int normRunNo = 0;
int runNo = 1;
int timeline = 1000;

//-----> hbook variables

float hcfitd_[25];
float pawc_[MEMH];
int quest_[100];

//-----> variables from calib database
float stolenWindow[NUM_TC];
float tagRatio[NUM_TC];
float tagRatioU[NUM_TC];

/*--------------------------- Function prototypes ---------------------------*/

void PrintUsage(char *processName);

/*---------------------------------------------------------------------------*/

main(int argc, char **argv) {

    FILE *syncptr = NULL;

    char *argptr;
    char mess[100];
    char *outfile = NULL;
    char *tripFile = NULL;
    char *syncFile = NULL;
    char *dir = getenv("CLAS_PARMS");
    char map[100];

    clasHEAD_t *HEAD = NULL;
    clasTBTR_t *TBTR = NULL;
    clasTGBI_t *TGBI = NULL;

    int F_false = 0;
    int F_true = -1;
    int nmax = 300;
    int i;
    int pidGroup = 1;  // bid/pid banks group number
    int syncHigh = 0;
    int syncLow = 0;

    //-----> flags

    int batch = 0;
    int leftoverSync = 0;
    int rebuild = 1;

    uint32 phyHighTime = 0;
    uint32 syncHighTime = 0;
    uint32 syncLowTime = 0;
    uint32 syncInterval = 0;

    signal(SIGINT, ctrlCHandle);
    signal(SIGHUP, ctrlCHandle);

    set_level_(&F_false, &F_false, &F_false, &F_false, &F_false);

    /*-------------------------------------------------------------------------*/

    if(argc == 1) // No arguments after calling program
        PrintUsage(argv[0]);

    for(i = 1; i < argc; i++) { // loop over the switches

        argptr = argv[i];

        if(*argptr == '-') {

            argptr++;

            switch(*argptr) {

            case 'B':
                pid = 1;
                doParticles = 1;
                doRawTagger = 1;
                doExclusive = 1;
                break;

            case 'F':
                runGatedClockFreq = atoi(++argptr);
                break;

            case 'M':
                max=atoi(++argptr);
                break;

            case 'N':
                normRunNo = atoi(++argptr);
                break;

            case 'P':
                pid = 1;
                doParticles = 1;
                break;

            case 'R':
                rebuild = 0;
                break;

            case 'T':
                doRawTagger = 1;
                break;

            case 'E':
                Ecorr = 1;
                break;

            case 'b':
                batch = 1;
                break;

            case 'c':
                clocklt = 1;
                break;

            case 'e':
                doExclusive = 1;
                break;

            case 'f':
                fileNo = atoi(++argptr);
                break;

            case 'h':
                PrintUsage(argv[0]);
                break;

            case 'l':
                timeline = atoi(++argptr);
                break;

            case 'n':
                normRun = 1;
                break;

            case 'o':
                outfile = ++argptr;
                break;

            case 'p':
                doParticles = 1;
                break;

            case 's':
                startCounter = 1;
                break;

            case 't':
                trip = 1;
                tripFile = ++argptr;
                open_trip_file(tripFile);
                break;

            case 'y':
                syncFile = ++argptr;
                syncptr = fopen(syncFile, "r");
                if(syncptr)
                    fprintf(stderr, "Opening sync file: %s\n", syncFile);
                else {
                    fprintf(stderr,"\n Unable to open sync file: %s\n", syncFile);
                    fflush(stderr);
                    exit(0);
                }
                syncCor = 1;
                break;

            default:
                fprintf(stderr, "Unrecognized argument: [-%s]\n\n", argptr);
                PrintUsage(argv[0]);
                break;

            } // switch(*argptr)
        } // if(*argptr == '-')
    } // for(i = 1; i < argc; i++)

    // initalize the bos banks

    bnames_(&nmax);
    initbos();
    configure_banks(stderr, 0);

    sprintf(map, "%s/Maps/NORM.map", dir);

    for(i = 1; i < argc; ++i) {

        argptr = argv[i];

        if(*argptr != '-') {

            sprintf(mess, "OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);

            if(!fparm_c(mess)) {

                fprintf(stderr, "%s: Unable to open file \'%s\': %s\n\n",
                        argv[0], argptr, strerror(errno));

                exit(0);
            }

            if(rebuild && !pid) {

                int taco_tagr = 0;
                tagtcl_set_def_();
                tag_init_();
                tagtcl_.Tagger_energy_correct = Ecorr;
            }

            /* main event loop */

            while((max ? Nevents < max : 1) && getBOS(&bcs_, 1, "E")) {

                Nevents++;

                if(HEAD = getBank(&bcs_, "HEAD")) {

                    runNo = HEAD->head[0].nrun;

                    ConfigEvent(runNo, pidGroup);

                    if(Nevents == 1) {

                        if(pid)
                            initGPID(runNo);

                        getMapInfo();

                        hini(outfile);
                    }

                    if(HEAD->head[0].type > 0 && HEAD->head[0].type < 10) {

                        /* physics event */

                        // variable that just overwrites itself to get the last interrupt
                        // time in the scaler interval used for sync splitting
                        // This needs to be outside of the trip requirement or else it
                        // is broken for the first no trip interval when a split of
                        // the sync interval need to be performed

                        if(TGBI = getBank(&bcs_, "TGBI"))
                            phyHighTime = TGBI->tgbi[0].interrupt_time;

                    }

                    // If sync switch is used, gflux will correct the scaler interval
                    // time for lost time due to bad sync intervals

                    if(syncptr && HEAD->head[0].type > 0
                            && HEAD->head[0].type < 10
                            && HEAD->head[0].nevent >= syncHigh) {

                        // Checking for a leftover split interval before reading in the
                        // next sync interval

                        if(leftoverSync) {

                            syncIntervalCumulative += syncInterval;

                            syncInterval = 0;

                            leftoverSync = 0;
                        }

                        // Accumulating the lost sync times that fall between two
                        // scaler intervals.  This is needed if there are multiple
                        // sync intervals inside of one scaler interval

                        if(trip && syncLow >= trip_data_.first_in_scaler_interval
                                && syncHigh < trip_data_.last_in_scaler_interval) {

                            syncIntervalCumulative += syncInterval;

                            syncInterval = 0;
                        }

                        // Reading in the next bad sync interval

                        if(fscanf(syncptr, "%d %d %u %u\n", &syncLow, &syncHigh,
                                  &syncLowTime, &syncHighTime) != EOF) {

                            if(!batch)
                                fprintf(stderr, "will skip out of sync events from %d to %d\n",
                                        syncLow, syncHigh);

                            if(syncHighTime >= syncLowTime)
                                syncInterval = syncHighTime - syncLowTime;
                            else
                                syncInterval = OVERFLOW_32BIT - syncLowTime + syncHighTime;

                        }

                        else {

                            close(syncptr);
                            syncptr = NULL;
                        }
                    } // if(syncptr && HEAD->head[0].type > 0 ...

                    if(HEAD->head[0].type == 10) { /* scaler event */

                        Ns++;

                        if(syncptr && trip
                                && syncLow >= trip_data_.first_in_scaler_interval
                                && syncLow < trip_data_.last_in_scaler_interval) {

                            // Checking one more time since its possible for multiply sync
                            // interval in a row where the last interval has a scaler event
                            // inbetween.  This case will not have the last sync interval
                            // included in the correction without this final check

                            if(syncHigh <= trip_data_.last_in_scaler_interval) {

                                syncIntervalCumulative += syncInterval;

                                syncInterval = 0;
                            }

                            // scaler event falls inbetween a sync skip interval, hence
                            // breaking up the sync time into two parts using the time
                            // from the last physics event as the split time

                            if(syncHigh > trip_data_.last_in_scaler_interval) {

                                fprintf(stderr, " sync interval will be split\n");

                                if(phyHighTime >= syncLowTime)
                                    syncInterval = phyHighTime - syncLowTime;
                                else
                                    syncInterval = OVERFLOW_32BIT - syncLowTime + phyHighTime;

                                syncIntervalCumulative += syncInterval;

                                if(syncHighTime >= phyHighTime)
                                    syncInterval = syncHighTime - phyHighTime;
                                else
                                    syncInterval = OVERFLOW_32BIT - phyHighTime + syncHighTime;

                                leftoverSync = 1;
                            }
                        } //if(syncptr && trip ...

                        runTime = 0.;
                        runTimeFU = 0.;
                        clock = 0.;

                        // The function getTime must be called for every scaler interval,
                        // even for tripped intervals since the function uses static
                        // variables

                        getTime();

                        if(trip ? TRIP() == 0 : 1) {

                            NnonTrip++;

                            fluxCal();

                            if(Ns < timeline)
                                fillScalerHistos();

                            if(clock)
                                getRateST(); // These rates are calculated from the scalers
                        }

                        NeventsPHY = 0; // These varables are per scaler interval
                        NeventsTRK = 0;
                        Ntagr = 0;
                        Nstr = 0;

                        syncIntervalCumulative = 0;

                    } // if(HEAD->head[0].type == 10){
                    else if(HEAD->head[0].type > 0 &&
                            HEAD->head[0].type < 10 &&
                            (trip ? TRIP() == 0 : 1) &&
                            (syncptr ? HEAD->head[0].nevent < syncLow : 1)) {

                        /* physics event */

                        NeventsPHY++;

                        if(TBTR = getBank(&bcs_, "TBTR"))
                            NeventsTRK++;

                        // variable that just overwrites itself to get the last interrupt
                        // time in the scaler interval used for sync splitting

                        //if(TGBI = getBank(&bcs_, "TGBI"))
                        //phyHighTime = TGBI->tgbi[0].interrupt_time;

                        if(pid) { // Rebuilding tagger banks, vertex and pid banks

                            dropAllBanks(&bcs_, BID_BANKS);
                            bankList(&bcs_, "E+", BID_BANKS);
                            make_BID_banks(pidGroup);
                            dropAllBanks(&bcs_, "PARTMVRTGPID");
                            bankList(&bcs_, "E+", "PARTMVRTGPID");
                            make_mvrt();
                            make_PART_group(pidGroup);
                            makeGPID(pidGroup, 0);
                        }
                        else if(rebuild) { // ONLY rebuilding the tagger banks

                            dropAllBanks(&bcs_, TAG_BANKS);
                            make_CL01_bank();
                            tag_evnt_();
                        }

                        fillHistos();

                    } // else if(HEAD->head[0].type > 0 && ...
                } // if(HEAD = getBank(&bcs_, "HEAD"))

                if((Nevents % 1000 == 0) && !batch) {

                    fprintf(stderr, "\t%d\r", Nevents);
                    fflush(stderr);
                }

                dropAllBanks(&bcs_, "E");
                cleanBanks(&bcs_);
            } // End of event loop

            if(!batch)
                fprintf(stderr, "# of events read: %d\n", Nevents);

            sprintf(mess, "CLOSE BOSINPUT UNIT=1");
            fparm_c(mess);

        } // if(*argptr != '-')
    } // for(i = 1; i < argc; ++i)

    fprintf(stderr, "\nTotal number of events read: %d\n\n", Nevents);

    if(!normRun) {

        finalCal();

        if(doParticles) { // Producing normalized yields for both PART and GPID

            // normYield Function call(numBins, histoIndex, pidType)
            // histoIndex:
            // PART:  TC 2000, EC 3000, ERG 4000
            // GPID:  TC 12000, EC 13000, ERG 14000
            // pidType: PART 1, GPID 2

            normYield(NUM_TC, 2000, 1);
            normYield(NUM_EB, 3000, 1);
            normYield(NUM_ERG, 4000, 1);
            normYield(NUM_TC, 12000, 2);
            normYield(NUM_EB, 13000, 2);
            normYield(NUM_ERG, 14000, 2);
        }
    }
    else
        taggingRatio();

    cleanUp();

    exit(1);
}

/*---------------------------------------------------------------------------*/

void PrintUsage(char *processName) {

    fprintf(stderr, "Usage: %s [-Options] file1 [file2] etc...\n\n", processName);
    fprintf(stderr, "  Options:\n");
    fprintf(stderr, "\t-B\t\tBloated mode(All histograms) equivalent to -P -T -e\n");
    fprintf(stderr, "\t-F[#]\t\tRun gated clock frequency in KHz, default 10 KHz\n");
    fprintf(stderr, "\t-M[#]\t\tProcess only # number of events\n");
    fprintf(stderr, "\t-N[#]\t\tNormalize to this run, instead of using map\n");
    fprintf(stderr, "\t-P\t\tRebuild PID with particle histograms\n");
    fprintf(stderr, "\t-R\t\tDo NOT rebuild TAGR bank, by default it does\n");
    fprintf(stderr, "\t-T\t\tRaw tagger histograms from TAGE, TAGT, and TAGI\n");

    fprintf(stderr, "\t-E\t\tapply tagger energy correction (default: no correction)\n");

    fprintf(stderr, "\t-b\t\tBatch mode(no printout on screen)\n");
    fprintf(stderr, "\t-c\t\tClock based DAQ livetime, default event based\n");
    fprintf(stderr, "\t-e\t\tMake exclusive reaction histograms\n");
    fprintf(stderr, "\t-f\t\tFile number, necessary if you want to keep txt files\n");
    fprintf(stderr, "\t-l[#]\t\tScaler intervals in timeline histograms, default 50\n");
    fprintf(stderr, "\t-n\t\tProcess a normalization run\n");
    fprintf(stderr, "\t-o<outfile>\tOutput hbook file name\n");
    fprintf(stderr, "\t-p\t\tParticle histograms without rebuilding PID\n");
    fprintf(stderr, "\t-s\t\tStart counter histograms from ST1 and STR\n");
    fprintf(stderr, "\t-t<file>\tTrip file\n");
    fprintf(stderr, "\t-y<file>\tSynch event mode (skip events)\n");

    exit(0);
}

/*------------------------------End of Program-------------------------------*/








