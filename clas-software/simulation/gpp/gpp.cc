#include "gpp.h"

#define USE(var) static void * use_##var = (void *) &var

static char crcsid[] = "$Id: gpp.cc,v 1.38 2009/09/13 09:52:46 pmatt Exp $";
USE(crcsid);   // make sure it is not optimized away

int main(int argc,char **argv)
{
//nick's version
  char *argptr;
  int MaxBanks       = 1000;
  int icycle         = 0;
  int Nevents        = 0;
  int max            = 10000000;
  int RunNo          = 1;
  int verbose        = 0;
  char *outfile      = NULL;
  char *tagfile      = NULL;
  char out[200];
  char mess[400];

  int Batch          = 0;
  int Ntrigger       = 0;
  int trigger_passed = 1;
  int drop_banks     = 1;
  int retTrig;
  int dummy;
  int trigtest       = 0;
  int TrigType[4]     = { 0x1, 0x3, 0x7, 0xf };
  int partflag        = 0;

  if(argc == 1)
    {
      PrintUsage(argv[0]);
      exit(0);
    }
  for (int i=1; i<argc; i++)
    {
      argptr = argv[i];
      if(*argptr == '-')
    {
      argptr++;
      switch (*argptr)
        {
        case 'h':
          PrintUsage(argv[0]);
          break;
        case 'M':
          max = atoi(++argptr);
          break;
        case 'R':
          RunNo = atoi(++argptr);
          break;
        case 'v':
          verbose = 1;
          break;
        case 'P':
          ProcessFlag = strtoul(++argptr, NULL, 0);
          break;
        case 'T':
          TrigFlag = strtoul(++argptr, NULL, 0);
          break;
        case 't':
          trigtest = 1;
        case 'E':
          EC_thresh = atof(++argptr);
          break;
        case 'C':
          CC_thresh = atof(++argptr);
          break;
        case 'I':
          ECInner_thresh = atof(++argptr);
          break;
        case 'D':
          drop_banks = atoi(++argptr);
          break;
        case 'f':
          ScaleFactor = atof(++argptr);
          break;
        case 'a':
          Reg1Scale = atof(++argptr);
          break;
        case 'b':
          Reg2Scale = atof(++argptr);
          break;
        case 'c':
          Reg3Scale = atof(++argptr);
          break;
        case 'A':
          AddTAGR =1;
          tagfile =  *(++argptr) ? argptr : (char *)"/dev/fd/1";
          fprintf(stderr,"TAGR input file: %s\n",tagfile);
          sprintf(out, "OPEN TAGINPUT UNIT=8 FILE=\"%s\" READ", tagfile);
          if (!fparm_c(out))
        {
          fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
          exit(1);
        }
          break;
        case 'Y':
          EFF = 1;
          break;
        case 'S':
          MEANS = 1;
          break;
         case 's':
           TAGRSync = 1;
          break;
        case 'o':
          outfile =  *(++argptr) ? argptr : (char *)"/dev/fd/1";
          fprintf(stderr,"Output file: %s\n",outfile);
          unlink(outfile);
          sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outfile);
          if (!fparm_c(out))
        {
          fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
          exit(1);
        }
          break;

        case 'B':
          Batch = 1;
          break;

        default:
          fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
          PrintUsage(argv[0]);
          break;
        }
    }
    }

  srand((unsigned int) time(0));  // initializes random number generator

  cout<<"runno: "<<RunNo<<endl;


  // initialize process flags ------------------------------------------------------------
  if(ProcessFlag & INCLUDE_DC && EFF!=1)
    {
      IncludeDC = 1;
      cout << "Removing DC dead wires..." << endl;
    }

  if(EFF==1) // Overwriting Include_DC
    {
      IncludeDC = 0;
      cout << "Removing DC hits according to efficiency in GPP map, DC_WIRE." << endl;
    }

  if(MEANS==1) cout << "Mean of DOCA in each region will be shifted according to values in GPP.map, Reg#_mean" << endl;

  if(ProcessFlag & SMEAR_DC)
    {
      SmearDC = 1;
      cout << "Smearing DOCA values..." << endl;
      cout << "\tRegion 1 Scale Factor = " << Reg1Scale << endl;
      cout << "\tRegion 2 Scale Factor = " << Reg2Scale << endl;
      cout << "\tRegion 3 Scale Factor = " << Reg3Scale << endl;
    }
  if(ProcessFlag & INCLUDE_SC)
    {
      IncludeSC = 1;
      cout << "Removing SC dead paddles..." << endl;
    }
  if (ProcessFlag & SMEAR_SC)
    {
      SmearSC = 1;
      cout << "Smearing SC tdcl/tdcr values..." << endl;
      cout << "\ttdcl/tdcr Scale Factor = " << ScaleFactor << endl;
    }
  if(ProcessFlag & INCLUDE_RF)
    {
      IncludeRF = 1;
      cout << "Creating CALL Bank..." << endl;
    }
  if(ProcessFlag & SMEAR_TAGR)
    {
      SmearTAGR = 1;
      cout << "Creating CALL Bank..." << endl;
    }
  if(ProcessFlag & REMOVE_BAD_TAGR)
    {
      RemoveBadTAGR = 1;
      cout << "Creating CALL Bank..." << endl;
    }
  if(ProcessFlag & INCLUDE_IC)
    {
      IncludeIC = 1;
      cout << "Removing IC dead channels..." << endl;
    }
  if(ProcessFlag & TAGRSync)
    {
      cout << "Shifting TAGR.thpo of MC photon to photon time at the center of the target..." << endl;
    }
  if(!ProcessFlag & TAGRSync)
    TAGRSync = 0;
  if(ProcessFlag & SMEAR_IC)
    {
      SmearIC = 1;
      //      ic_noise=0;ic_noiseRMS=0.009;
      //      ic_apar=0.008;ic_bpar=0.0349; ic_cpar=0.0230;
      cout << "Smearing IC adc and tdc values..." << endl;
      printf("IC smearing parameters a=%6.4f MeV b=%6.4f (percent) c=%6.4f (percent)\n time smearing:%4.1f ns",ic_apar*1000,ic_bpar,ic_cpar,ic_timeRMS);
    }

  cout << endl;
  if (trigtest) TrigFlag = 0x0;

  // initialize trigger flags -------------------------------------------------------------
  if(TrigFlag & EC_IN_TRIG)
    {
      ectrig = 1;
      fprintf(stderr, "EC Threshold: %4.1f mV.\n", EC_thresh);
    }
  if(TrigFlag & ECINNER_IN_TRIG)
    {
      ecinnertrig = 1;
      fprintf(stderr, "EC Inner Threshold: %4.1f mV.\n", ECInner_thresh);
    }
  if(TrigFlag & CC_IN_TRIG)
    {
      cctrig = 1;
      fprintf(stderr, "CC Threshold: %4.1f mV.\n", CC_thresh);
    }
  if(TrigFlag & CC_BIT1_NOT_IN_TRIG)
    {
      notb1trig = 1;
      min_cc_bit = NOTB1_MIN_CC_BIT;
      fprintf(stderr, "CC Bit 1 not in trigger.\n");
    }

  cout << endl << "Using DC drift velocity = " << DRIFT_VELOCITY<< " microns/nanosec.\n\n" << endl;


  // Convert mV Thresholds to ADC channels and apply sampling fraction to ECs -------------------
  EC_thresh      = EC_thresh      * MV_2_ADC_EC;
  EC_thresh      = EC_thresh      * EC_SAMPLING_FRAC;
  ECInner_thresh = ECInner_thresh * MV_2_ADC_EC;
  ECInner_thresh = ECInner_thresh * EC_SAMPLING_FRAC;
  CC_thresh      = CC_thresh      * MV_2_ADC_CC;

  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,0);
  hini();

  for (int i = 1;i < argc; ++i)
    {
      argptr = argv[i];
      if (*argptr != '-')
    {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      else
        {
          while((Nevents < max) && getBOS(&bcs_,1, (char*) "E") )
        {
          Nevents++;

          if(ectrig || cctrig || ecinnertrig)
            {
              trigger_passed = 0;
              retTrig=0;
              if( (trigger_passed = TrigSect(&retTrig)) ) Ntrigger++;

            }
          if (trigtest)
            {

               partflag = 0;
              for(int itrig =0; itrig < 4; ++itrig)
            {
              TrigFlag = TrigType[itrig];
              // initialize trigger flags
              ectrig      = 0;
              ecinnertrig = 0;
              cctrig      = 0;
              notb1trig   = 0;
              min_cc_bit  = MIN_CC_TRIG_BIT;
              if(TrigFlag & EC_IN_TRIG)      ectrig = 1;
              if(TrigFlag & ECINNER_IN_TRIG) ecinnertrig = 1;
              if(TrigFlag & CC_IN_TRIG)      cctrig = 1;
              if(TrigFlag & CC_BIT1_NOT_IN_TRIG)
                {
                  notb1trig = 1;
                  min_cc_bit = NOTB1_MIN_CC_BIT;
                }
              if(TrigSect(&dummy) > 0) partflag += (int) pow(2.,itrig);
            }
              ProcessPART(partflag);
              hf1(80,float(partflag),1.);
              TrigFlag    = 0x0;
              ectrig      = 0;
              ecinnertrig = 0;
              cctrig      = 0;
              notb1trig   = 0;
              min_cc_bit  = MIN_CC_TRIG_BIT;

            }
          if(ProcessEvent(RunNo,verbose))
            {
              clasHEAD_t *HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD");
              if(HEAD) HEAD->head[0].trigbits = retTrig;
              if (drop_banks && (trigger_passed == 0) ) dropAllBanks(&bcs_,(char*) "CALLEC  CC  SC  DC0 DOCA");
              putBOS(&bcs_, 7, (char*) "E");

              if(!Batch && (Nevents % 200 == 0))
            {
              if(ectrig || cctrig || ecinnertrig)
                {
                  fprintf(stderr,"# of Events processed: %d, # of Events trigger-passed: %d\r",Nevents, Ntrigger);
                  fflush(stderr);
                }
              else
                {
                  fprintf(stderr,"# of Events processed: %d\r",Nevents);
                  fflush(stderr);
                }
            }

            }
          dropAllBanks(&bcs_,(char*) "E");
          cleanBanks(&bcs_);
        }
        } // end of if/else   (File opened properly)
      if (!fparm_c((char*) "CLOSE BOSINPUT")) fprintf(stderr,"%s: Unable to close file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
    }
    } // end of for (arguments)

  if(ectrig || cctrig || ecinnertrig)
    {
      fprintf(stderr,"# of Events processed: %d, # of Events trigger-passed: %d\r",Nevents, Ntrigger);
      fflush(stderr);
    }
  else
    {
      fprintf(stderr,"# of Events processed: %d\r",Nevents);
      fflush(stderr);
    }
  fprintf(stderr,"\n# of events written: %d\n",Nevents);

  // close file
  putBOS(&bcs_, 7, (char*) "0"); // flush last buffer
  sprintf(mess,"CLOSE BOSOUTPUT UNIT=7");
  fparm_c(mess);
  fparm_c((char*) "CLOSE TAGRINPUT UNIT=8");
  hrout(0,icycle,(char*) " ");
  hldir_((char*) " ", (char*) " ", 1L, 1L); // don't remove this line
  hrend_((char*) "esr", 3L);
}

void PrintUsage(char *processName)
{
  cout << "Usage: " << processName << " [options] inputFile 1 [inputFile2] etc. \n\n";
  cout << "\t\t Writes trigger bits into HEAD trigbits (latch1) field, whether\n";
  cout << "\t\t the event passes the trigger or not.\n\n";
  cout << "\t options:\n";
  cout << "\t -P[#]\tBitwise Process Flags, as defined below:" << endl;
  cout << "\t\t\t 0x" << hex << INCLUDE_DC << "\t DC - Remove dead wires"<< endl;
  cout << "\t\t\t 0x" << hex << SMEAR_DC << "\t Smear DC values" << endl;
  cout << "\t\t\t 0x" << hex << INCLUDE_SC << "\t SC - Remove dead paddles" << endl;
  cout << "\t\t\t 0x" << hex << SMEAR_SC << "\t Smear SC values" << endl;
  cout << "\t\t\t 0x" << hex << INCLUDE_RF << "\t Create CALL Bank" << endl;
  cout << "\t\t\t 0x" << hex << SMEAR_TAGR << "\t Smear Tagger timeing" << endl;
  cout << "\t\t\t 0x" << hex << REMOVE_BAD_TAGR << "\t Remove bad tagger conters" << endl;
  cout << "\t\t\t 0x" << hex << INCLUDE_IC << "\t IC - Remove dead IC channels" << endl;
  cout << "\t\t\t 0x" << hex << SMEAR_IC << "\t Smear IC values" << endl;
  cout << "\t\t   Default: 0x" << hex << 31 << " (all but tagger activated)" << endl;
  cout << "\t-T[#]\t Bitwise Trigger Flags, as defined below:" << endl;
  cout << "\t\t\t 0x" << hex << EC_IN_TRIG << "\t EC in trigger"<< endl;
  cout << "\t\t\t 0x" << hex << ECINNER_IN_TRIG << "\t ECinner in trigger" << endl;
  cout << "\t\t\t 0x" << hex << CC_IN_TRIG << "\t CC in trigger" << endl;
  cout << "\t\t\t 0x" << hex << CC_BIT1_NOT_IN_TRIG << "\t Starts CC trigger after CC bit 1" << endl;
  cout << "\t\t   Default: 0x" << hex << 0 << " (none activated)" << endl;
  cout << "\t-t\t Determine possible trigger types for a given event (PART bank only). Overide -T option" << endl;
  cout << "\t-o filename\toutput to filename\n";
  cout << "\t-A filename\tget TAGR banks from filename\n";

  cout << "\t-M[#]\t Process  only (#) number of events\n";
  cout << "\t-R[#]\t Use Run Number (#) to overlay on the Monte Carlo banks (def=1)\n";
  cout << "\t-v\t verbose mode - print dead wires or efficiencies for each wire." << endl;
  cout << "\t-E\t EC threshold in mV" << endl;
  cout << "\t-I\t EC inner threshold in mV" << endl;
  cout << "\t-C\t CC threshold in mV" << endl;
  cout << "\t-D[#]\t Drop (#=1 default) or don't drop (#=0) raw banks for failed events.\n";
  cout << "\t-f\t scale factor for SC tdcl/tdcr smear" << endl;
  cout << "\t-a\t scale factor for DOCA smear in Region 1" << endl;
  cout << "\t-b\t scale factor for DOCA smear in Region 2" << endl;
  cout << "\t-c\t scale factor for DOCA smear in Region 3" << endl;
  cout << "\t-Y\t Drop DC hits according to efficiency in GPP map, DC_WIRE." << endl;
  cout << "\t-S\t Shift doca Mean according to GPP map." << endl;
  cout << "\t-s\t Shift MC TAGR photon times from the photon time at the reaction vertex to the photon time at the center of the target." << endl;
  cout << "\t-h\t This information\n";
  exit(0);
}

void hini()
{
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat;
  char *hist;
  hist = (char*) "gpp.hbook";
  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, (char*) "esr", hist , (char*) "NQ", &lrec, &istat, 3L, strlen(hist), 1L);
  book_histos();
  return;
}

void book_histos()
{
  hbook1(80,  (char*) "trigger type",                                          17,         0.0,    17.0,                       0.);
  hbook1(101, (char*) "doca smearing([m]m) in region 1",                       1000,   -3000.0,  3000.0,                       0.);
  hbook1(102, (char*) "doca smearing([m]m) in region 2",                       1000,   -3000.0,  3000.0,                       0.);
  hbook1(103, (char*) "doca smearing([m]m) in region 3",                       1000,   -3000.0,  3000.0,                       0.);
  hbook1(201, (char*) "tdcl smearing (ns)",                                    1000,      -1.0,     1.0,                       0.);
  hbook1(202, (char*) "tdcr smearing (ns)",                                    1000,      -1.0,     1.0,                       0.);
  hbook2(301, (char*) "Scintillator length (cm) vs. Resolution (ns) for tdcl", 100,        0.0,   500.0, 100,    -1.0,    1.0, 0.);
  hbook2(302, (char*) "Scintillator length (cm) vs. Resolution (ns) for tdcr", 100,        0.0,   500.0, 100,    -1.0,    1.0, 0.);
  hbook2(401, (char*) "doca ([m]m) vs. doca smearing ([m]m) in region 1",      100,    -8000.0,  8000.0, 100 ,-2500.0, 2500.0, 0.);
  hbook2(402, (char*) "doca ([m]m) vs. doca smearing ([m]m) in region 2",      100,   -14000.0, 14000.0, 100, -3500.0, 3500.0, 0.);
  hbook2(403, (char*) "doca ([m]m) vs. doca smearing ([m]m) in region 3",      100,   -22000.0, 22000.0, 100, -4000.0, 4000.0, 0.);
  hbook1(500, (char*) "RF tdc values (ns)",                                    10000,      0.0,   100.0,                       0.);
  hbook1(501, (char*) "RF tdc values (ns) with smearing",                      10000,      0.0,   100.0,                       0.);
  hbook1(502, (char*) "RF tdc smearing values (ns)",                           1000,     -0.02,    0.02,                       0.);
  hbook1(600, (char*) "TAGR T_pho All",                                        800,       -20.,     20.,                       0.);
  hbook1(601, (char*) "TAGR T_pho accidental",                                 800,       -20.,     20.,                       0.);
  hbook1(602, (char*) "TAGR E_id",                                             800,         0.,    800.,                       0.);
  hbook2(603, (char*) "TAGR MC photon time shift (ns) vs Vertex Z (cm)",        150,     -110.0,   40.0, 100,    -2.0,    2.0, 0.);
  hbook2(701, (char*) "IC idx vs idy", 100,   0.0, 23.0, 100, 0.0, 23.0, 0.);
  hbook1(702, (char*) "IC adc values (GeV)", 1000,   0.0, 5.00, 0.);
  hbook1(703, (char*) "IC adc values smeared (GeV)", 1000,   0.0, 5.00, 0.);
  hbook1(704, (char*) "IC tdc values (ns)", 300,   0.0, 150, 0.);
  hbook1(705, (char*) "IC tdc values smeared (ns)", 300,   0.0, 150, 0.);

  char title[80];
  for( int sec=1; sec<=6; sec++ ) {
    sprintf(title,"SC TDC Right Paddles - Before GPP - Sector %d",sec);
    hbook1(800+sec, title, NumBars, 0.5, NumBars+0.5, 0.);
    sprintf(title,"SC TDC Left Paddles - Before GPP - Sector %d",sec);
    hbook1(810+sec, title, NumBars, 0.5, NumBars+0.5, 0.);
    sprintf(title,"SC TDC Right Paddles - After GPP - Sector %d",sec);
    hbook1(850+sec, title, NumBars, 0.5, NumBars+0.5, 0.);
    sprintf(title,"SC TDC Left Paddles - After GPP - Sector %d",sec);
    hbook1(860+sec, title, NumBars, 0.5, NumBars+0.5, 0.);
  }
}

void initRun(int RunNo,int verbose)
{
  for(int sec=1; sec <= 6; sec++)
    {
      make_DDLY_bank(RunNo, sec);
      make_DCDW_bank(RunNo, sec);
    }
  // SC setup: code version, gemetry, status
  set_sc_version_constants(RunNo);
  int sc_flag = get_sc_version_flag(RunNo);
  NumBars = (sc_flag==1)? 48: 57;

  load_map_vars(RunNo);           // load map variables into arrays

  make_SCG_banks(RunNo);


  sc_status_setup(RunNo);


  // get length of SC paddles from SCG bank
  clasSCG_t *SCG = (clasSCG_t*) getGroup(&wcs_,"SCG ",1);
  if( SCG ) {
    for( int i=0; i<NumBars; i++)
      SC_length[i] = get_bar_length(SCG,i+1);

  }
  else if( NumBars==NumBars_total ) {
    for (int i=48; i<NumBars; i++)
      SC_length[i] = SC_length[47];   //kludge to initialize if geometry bank missing
  }
  if (verbose) cout << "\n\nRun " << RunNo << " initialized\n";
}

int ProcessEvent(int RunNo, int verbose)
{
  static int CurrentRun = 0;
  clasHEAD_t *HEAD;
  if( (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD")) )
    {
      HEAD->head[0].nrun = RunNo;
      if(verbose == 2) cerr << HEAD->head[0].nrun<< " " << HEAD->head[0].nevent << endl;
      if(CurrentRun != HEAD->head[0].nrun)
    {
      RunNo ? initRun(RunNo, verbose) : initRun(HEAD->head[0].nrun, verbose);
      CurrentRun = HEAD->head[0].nrun;
    }

      if(IncludeDC || SmearDC || EFF==1)        ProcessDC(verbose);
      if(IncludeSC || SmearSC)                  ProcessSC();
      if(IncludeIC || SmearIC)                  ProcessIC();
      if(IncludeRF)                             ProcessRF();
      if(SmearTAGR || RemoveBadTAGR || AddTAGR || TAGRSync) ProcessTAGR(RunNo);

    }
  return(1);
}

// DC STUF F-------------------------------------------------------------------
int ProcessDC(int verbose)
{
  int isl2, isl, ireg, ndc0;
  dc0_t  dc0[DC_WIRES_SEC];
  doca_t doca[DC_WIRES_SEC];

  float driftVelocity = DRIFT_VELOCITY;
  float smearValue;
  int   ddly_ind;
  float propTime;
  float value1, value2;   // before - after smaring
  float shifted_mean;     // shifted  mean
  float groups_ran[100] ; // random number for each group. First element: no group (indipendent wires). All other groups will have correlated efficiency.

  int   group_index;      // efficiency in GPP map is added by group # * 1000 ; So an efficiency of 2000.35 means all wires in group 2 have 35% eff .


  for (int isec = 1; isec <= NSEC; ++isec)
    {
      clasDC0_t  *DC0  = (clasDC0_t  *)getGroup(&bcs_, "DC0 ", isec);
      clasDOCA_t *DOCA = (clasDOCA_t *)getGroup(&bcs_, "DOCA", isec);
      clasDDLY_t *DDLY = (clasDDLY_t *)getGroup(&wcs_, "DDLY", isec);
      ndc0 = 0;

      if (DC0 && DOCA && DDLY)
    {
      for(int i=0; i<100; i++) groups_ran[i] = -1;                                                // initializing random numbers for all groups
      for (int i = 0; i < DC0->bank.nrow; ++i)
        {
          ddly_ind   = id2ddly(DC0->dc0[i].id);                                                       // absolute wire number
          propTime   = DC0->dc0[i].tdc/DC_TDC_TO_NS - fabs((float) DOCA->doca[i].doca)/driftVelocity; // in nanosecs
          value1     = DOCA->doca[i].doca;
          isl2       = (int) (DC0->dc0[i].id/256) - 1;
          isl        = (int) (isl2 / 6) + 1;
          ireg       = (int) ((isl-1)/2) + 1;
          smearValue   = 0.0;
          shifted_mean = 0.0 ;

          if(MEANS)    shifted_mean = get_mean( ireg, value1);
          if(SmearDC)  smearValue   = get_resol(ireg, value1);

          if(IncludeDC && EFF==0)
        {
          if(DDLY->ddly[ddly_ind].stat == DC_GOOD_WIRE)
            {
              DOCA->doca[i].doca = (int) SmearFnc(GAUSSIAN_MODE, value1, smearValue);
              DC0->dc0[i].tdc    = (uint16) DC_TDC_TO_NS * (int) (fabs((float) DOCA->doca[i].doca/driftVelocity) + propTime); // assume 2 channels/nanosec
              dc0[ndc0]          = DC0->dc0[i];
              doca[ndc0]         = DOCA->doca[i];
              value2             = DOCA->doca[i].doca;
              hf1(100+ireg, value1-value2,         1.);
              hf2(400+ireg, value1, value1-value2, 1.);
              ndc0++;
            }
        }
          else if(!IncludeDC && EFF==1)
        {
          group_index         = ddly_ind/1000;
          //   cout << value1 << endl;
          if(groups_ran[group_index] == -1 || group_index == 0) groups_ran[group_index] = drand48();
          DOCA->doca[i].doca  = (int) SmearFnc(GAUSSIAN_MODE, value1, smearValue)  - shifted_mean;
          DC0->dc0[i].tdc     = (uint16) DC_TDC_TO_NS * (int) (fabs((float) DOCA->doca[i].doca/driftVelocity) + propTime);
          dc0[ndc0]           = DC0->dc0[i];
          doca[ndc0]          = DOCA->doca[i];
          value2              = DOCA->doca[i].doca;
          hf1(100+ireg, value1-value2,         1.);
          hf2(400+ireg, value1, value1-value2, 1.);


          if(groups_ran[group_index] < fmod(Eff[isec-1][ddly_ind], 1000)) ndc0++;
          if(verbose) cout << "wire: " << ddly_ind << "   :" << fmod(Eff[isec-1][ddly_ind], 1000) << " " << Eff[isec][ddly_ind] << " " << "Group number " << group_index << endl;

        }

          else
        {
          DOCA->doca[i].doca = (int) SmearFnc(GAUSSIAN_MODE,value1,smearValue);
          DC0->dc0[i].tdc    = (uint16) DC_TDC_TO_NS * (int) (fabs((float) DOCA->doca[i].doca/driftVelocity) + propTime);
          dc0[ndc0]          = DC0->dc0[i];
          doca[ndc0]         = DOCA->doca[i];
          value2             = DOCA->doca[i].doca;
          hf1(100+ireg, value1-value2,         1.);
          hf2(400+ireg, value1, value1-value2, 1.);
          ndc0++;
        }

        }
    }

      dropBank(&bcs_,(char*) "DC0 ",isec);
      dropBank(&bcs_,(char*) "DOCA",isec);

      // careful! (sizeof(dc0_t)/4)*2 required for DC0 (B16) bank
      if(ndc0)
    {
      if( (DC0  =(clasDC0_t  *)makeBank(&bcs_,(char*) "DC0 ", isec,(sizeof(dc0_t)/4)*2,  ndc0)) ) for(int i=0; i < ndc0; i++) DC0->dc0[i]   = dc0[i];
      if( (DOCA =(clasDOCA_t *)makeBank(&bcs_,(char*) "DOCA", isec,(sizeof(doca_t)/4)*2, ndc0)) ) for(int i=0; i < ndc0; i++) DOCA->doca[i] = doca[i];
    }
    }
  return(1);
}


//SCSTUFF----------------------------------------------------------------------
int ProcessSC(){
  clasSC_t *SC = NULL;
  sc_t *sc_new = NULL;
  int nsc = 0, i = 0;
  int sec;
  const float conversion = 0.05;  // conversion factor { units:(ns/channel) }
  float sigma;
  int paddle_id;
  float value1, value2;
  float value1_new, value2_new;
  for(sec = 1; sec <= 6; ++sec){
    if ( (SC = (clasSC_t *)getGroup(&bcs_, "SC  ", sec)) ){
      if (! (sc_new = (sc_t *)malloc(sizeof(sc_t)* (SC->bank.nrow)))) {
    cerr << "Error allocating memory!" << endl;
    exit(1);
      }  // end of if
      nsc = SC->bank.nrow;
      memcpy(sc_new, SC->sc,sizeof(sc_t)* (SC->bank.nrow));

      dropBank(&bcs_, (char*) "SC  ", sec);

      for (i = 0; i < nsc; i++){
          //the line to change
      paddle_id = sc_new[i].id;
    if (IncludeSC) {

      if(sc_new[i].tdcr) hf1(800+sec,    paddle_id, 1.);  // right
      if(sc_new[i].tdcl) hf1(800+sec+10, paddle_id, 1.);  // left

    // look at LEFT side of paddle
      switch(sc_HDWR_channel_status(sec, sc_new[i].id)/10){

      case SC_STATUS_NO_ADC:
        sc_new[i].adcl = 0;
        break;
      case SC_STATUS_NO_TDC:
        sc_new[i].tdcl = 0;

        break;
      case SC_STATUS_DEAD:
        sc_new[i].adcl = sc_new[i].tdcl = 0;
        break;
      default:
        break;
      } // end of switch

      // look at RIGHT side of paddle
      switch(sc_HDWR_channel_status(sec, sc_new[i].id)%10){
      case SC_STATUS_NO_ADC:
        sc_new[i].adcr = 0;
        break;
      case SC_STATUS_NO_TDC:
        sc_new[i].tdcr = 0;
        break;
      case SC_STATUS_DEAD:
        sc_new[i].adcr = sc_new[i].tdcr = 0;
        break;
      default:
        break;
      }  // end of switch

   // booking histos after gpp on SC TDC:
   // look at LEFT side of paddle
     if(sc_new[i].tdcr) hf1(800+sec+50, paddle_id, 1.);  // right
     if(sc_new[i].tdcl) hf1(800+sec+60, paddle_id, 1.);  // left




      // smear times in the SC bank

     value1 = (float) sc_new[i].tdcl * conversion;
     value2 = (float) sc_new[i].tdcr * conversion;

      if (SmearSC) {
        // factor 2 is needed because when map is made factor 2 was missed.
        sigma = 2*ScaleFactor*Resolution[paddle_id-1];
          }
          else {
            sigma = 0.0;
      } // end of if-else

      value1_new = SmearFnc(GAUSSIAN_MODE,value1,sigma);
      value2_new = SmearFnc(GAUSSIAN_MODE,value2,sigma);

      hf1(201,value1-value1_new,1.);
      hf1(202,value2-value2_new,1.);
      hf2(301,SC_length[paddle_id-1],value1-value1_new,1.);
      hf2(302,SC_length[paddle_id-1],value2-value2_new,1.);

      sc_new[i].tdcl = (uint16) (fabs(value1_new / conversion));
      sc_new[i].tdcr = (uint16) (fabs(value2_new / conversion));
    }
    else {
          value1 = (float) sc_new[i].tdcl * conversion;
      value2 = (float) sc_new[i].tdcr * conversion;

      if (SmearSC) {
        // factor 2 is needed because when map is made factor 2 was missed.
        sigma = 2*ScaleFactor*Resolution[paddle_id-1];
            //sigma = 0.0;

          }
          else {
            sigma = 0.0;

      } // end of if-else

      value1_new = SmearFnc(GAUSSIAN_MODE,value1,sigma);
      value2_new = SmearFnc(GAUSSIAN_MODE,value2,sigma);

      hf1(201,value1-value1_new,1.);
      hf1(202,value2-value2_new,1.);

      hf2(301,SC_length[paddle_id-1],value1-value1_new,1.);
      hf2(302,SC_length[paddle_id-1],value2-value2_new,1.);

      sc_new[i].tdcl = (uint16) (fabs(value1_new / conversion));
      sc_new[i].tdcr = (uint16) (fabs(value2_new / conversion));
    }  // end of if
      }  // end of for

      if ( (SC = (clasSC_t *)makeBank(&bcs_, (char*) "SC  ", sec, sizeof(sc_t)/sizeof(uint16), nsc)) ){
        memcpy( SC->sc, sc_new,sizeof(sc_t)*nsc);
      } // end of if
      free(sc_new);
    } // end of if
  } // end of for

  return(1);

} // end of ProcessSC

// IC STUFF ---------------------------------------------------------------
int ProcessIC() {
  //Rustam Niyazov 10/04/2006
  //Remove dead IC calorimeter channels
  //Smear ADC and TDC values

  clasIC_t *IC = NULL;
  ic_t *ic_new = NULL;
  int GroupNo = 0;
  int id,tdc,adc;
  int adc_new,tdc_new;
  char sid[100];
  string sidx,sidy;
  int idx,idy,chan;
  //  float aconversion=0.8;//conversion 0.8 MeV= 1 channel
  //  float tconversion=0.5;//conversion 0.5 ns= 1 channel
  int nic=0;

  if(IC = (clasIC_t *)getGroup(&bcs_, (char*) "IC  ",GroupNo)){
    if (! (ic_new = (ic_t *)malloc(sizeof(ic_t)* (IC->bank.nrow)))) {
      cerr << "Error allocating memory!" << endl;
      exit(1);
    }  // end of if
    nic=IC->bank.nrow;
    memcpy(ic_new, IC->ic,sizeof(ic_t)* (IC->bank.nrow));
    dropBank(&bcs_, (char*) "IC  ", 0);

    for(int i=0; i< nic; i++){//loop thru hits
      id = (ushort)IC->ic[i].id;
      sprintf(sid, "%04x",id);
      sidx=sidy=sid;
      sidx.erase(0,2);sidy.erase(2,4);
      idx=strtol(sidx.c_str(),(char **)NULL,16);
      idy=strtol(sidy.c_str(),(char **)NULL,16);
      chan=idx+23*(idy-1)-1;
      adc =(ushort)IC->ic[i].adc;
      tdc =(ushort)IC->ic[i].tdc;
      float aconversion=ic_gain[chan]; //in GeV
      float tconversion=ic_tch[chan];  //in ns
      int chstatus=ic_stat[chan];
      float evalue=(float) (ic_new[i].adc-ic_pedestal[chan])*aconversion;
      float tvalue=(float) ic_new[i].tdc * tconversion;
      float evalue_new=0;
      float tvalue_new=0;

      if(SmearIC) {
    float noise_term=SmearFnc(GAUSSIAN_MODE,0.0,ic_apar);//smear noise around zero
        float stochastic_term=SmearFnc(GAUSSIAN_MODE,evalue,sqrt(ic_bpar*ic_bpar*evalue+ic_cpar*ic_cpar*evalue*evalue));
    //stochastic and leakage terms fluctuate around E as a function of energy in quadratures
    evalue_new=stochastic_term+noise_term;

    ic_new[i].adc = (uint16) (evalue_new/aconversion+ic_pedestal[chan]);
    tvalue_new = SmearFnc(GAUSSIAN_MODE,tvalue,ic_timeRMS);//Smeared tdc
    ic_new[i].tdc = (uint16) (fabs(tvalue_new/tconversion));
      }
      if(IncludeIC){
    if(ic_stat[chan]==0)
      {
        ic_new[i].adc=(uint16) 0.;
        ic_new[i].tdc=(uint16) 0.;
      }
      }
      adc_new=ic_new[i].adc;
      tdc_new=ic_new[i].tdc;
      //if(evalue>2.0){
    //printf("x:%2d y%2d chan:%3d adc=%d(%4.3f) adcn=%d(%4.3f) tdc=%d(%5.1f) tdcn=%d(%5.1f)\n",idx,idy,chan+1,adc,evalue,adc_new,evalue_new,tdc,tvalue,tdc_new,tvalue_new);
      //}
      hf2(701,float(idx),float(idy),1.);
      hf1(702,float(IC->ic[i].adc*aconversion),1.);
      hf1(703,float(ic_new[i].adc*aconversion),1.);
      hf1(704,float(IC->ic[i].tdc*tconversion),1.);
      hf1(705,float(ic_new[i].tdc*tconversion),1.);
    }//loop thru hits

    if( (IC = (clasIC_t *)makeBank(&bcs_, (char*) "IC  ", 0, sizeof(ic_t)/sizeof(uint16), nic)) ){
      memcpy( IC->ic, ic_new,sizeof(ic_t)*nic);
    } // end of if

    free(ic_new);
  }// end of if
  return(1);
}// end of ProcessIC

// RF STUFF ---------------------------------------------------------------
int ProcessRF() {
  call_t call;
  clasCALL_t *CALL;
  int i = int(Uniform(0.,41.));
  float rfsmear = Uniform(-0.015,0.015); // unit in nsec
  float temp;
  float temp1;

  formatBank((char*) "CALL",(char*) "B16");
  bankList(&bcs_,(char*) "E+",(char*) "CALL");

  temp  = (i*RF_STRUCTURE) + (5*RF_STRUCTURE) ; // unit in nsec
  temp1 = (i*RF_STRUCTURE) + (5*RF_STRUCTURE) + (rfsmear); // unit in nsec

  hf1(500,temp,1.);
  hf1(501,temp1,1.);
  hf1(502,rfsmear,1.);

  temp1 /= (call_calib.t1[0]);

  call.id =  (uint16) 6;
  call.tdc = (uint16) temp1;
  call.adc = (uint16) 0;

  if ( (CALL =(clasCALL_t *)makeBank(&bcs_,(char*) "CALL",0, sizeof(call_t)/sizeof(uint16) ,1)) ){
    CALL->call[0] = call;
  }

  return(1);
}  // end of ProcessRF

// TAGR STUFF ---------------------------------------------------------------
int ProcessTAGR(int RunNo) {
  static int CurrentRun = 0;
  static int TAGRevents = 0;
  clasTAGR_t *TAGR  = NULL;
  clasTAGR_t *DTAGR = NULL;
  clasMCTK_t *MCTK = NULL;
  clasMCVX_t *MCVX = NULL;
  clasPART_t *PART = NULL;
  tagr_t     *MTAGR = NULL;
  int Eid1;
  int Eid2;
  int Tid1;
  int Tid2;
  int i;
  int k;
  int locMCPhotonDroppedFlag = 0; //if accidental has same t_id as mc photon and lower time, mc photon is dropped

  //Do initialization first
  if(RunNo != CurrentRun) {
    CurrentRun = RunNo;
    if(tagr_setup(RunNo) != 0){
      fprintf(stderr,"ProcessTAGR -F- TAGR setup failed\n");
      exit(1);
    }
  }
  //Here goes real stuff
  if(TAGR = (clasTAGR_t *)getBank(&bcs_, (char*) "TAGR") ){
    if(SmearTAGR)
      TAGR->tagr[0].tpho =
    SmearFnc(GAUSSIAN_MODE,TAGR->tagr[0].tpho,tpho_sigma);
    if(RemoveBadTAGR) {
      //check E-id first
      if(TAGR->tagr[0].e_id %2) { //e_id is odd, no overlap
    Eid1 = (TAGR->tagr[0].e_id+1)/2;
    if(hwstatus_.E_hw_status[Eid1-1])
      TAGR->tagr[0].stat = 0; //drop it
      }
      else { //e_id is even, overlap
    Eid1 = TAGR->tagr[0].e_id/2;
    Eid2 = Eid1+1;
    if((hwstatus_.E_hw_status[Eid1-1] == 0) &&
       (hwstatus_.E_hw_status[Eid2-1] != 0)) {
      TAGR->tagr[0].e_id = Eid1*2-1;
      TAGR->tagr[0].erg = BeamEnergy *
        tagenergyboundaries_.Eaverage[TAGR->tagr[0].e_id-1];
    }
    else if((hwstatus_.E_hw_status[Eid1-1] != 0) &&
        (hwstatus_.E_hw_status[Eid2-1] == 0)) {
      TAGR->tagr[0].e_id = Eid2*2-1;
      TAGR->tagr[0].erg = BeamEnergy *
        tagenergyboundaries_.Eaverage[TAGR->tagr[0].e_id-1];
    }
    else if((hwstatus_.E_hw_status[Eid1-1] != 0) &&
        (hwstatus_.E_hw_status[Eid2-1] != 0)) {
      TAGR->tagr[0].stat = 0; //both dead, drop it
    }
      }
      //none of the above, keep it as it is.
      //now we will check T_id
      if(TAGR->tagr[0].t_id %2) { //t_id is odd, no overlap
    Tid1 = (TAGR->tagr[0].t_id+1)/2;
    if(hwstatus_.TL_hw_status[Tid1-1] |
       hwstatus_.TR_hw_status[Tid1-1])
      TAGR->tagr[0].stat = 0; //drop it
      }
      else { //t_id is even, overlap
    Tid1 = TAGR->tagr[0].t_id/2;
    Tid2 = Tid1+1;
    if((hwstatus_.TL_hw_status[Tid1-1] |
        hwstatus_.TR_hw_status[Tid1-1]) == 0 &&
       (hwstatus_.TL_hw_status[Tid2-1] |
        hwstatus_.TR_hw_status[Tid2-1]) != 0) {
      TAGR->tagr[0].t_id = Tid1*2-1;
    }
    else if((hwstatus_.TL_hw_status[Tid1-1] |
         hwstatus_.TR_hw_status[Tid1-1]) != 0 &&
        (hwstatus_.TL_hw_status[Tid2-1] |
         hwstatus_.TR_hw_status[Tid2-1]) == 0) {
      TAGR->tagr[0].t_id = Tid2*2-1;
    }
    else if((hwstatus_.TL_hw_status[Tid1-1] |
         hwstatus_.TR_hw_status[Tid1-1]) != 0 &&
        (hwstatus_.TL_hw_status[Tid2-1] |
         hwstatus_.TR_hw_status[Tid2-1]) != 0) {
      TAGR->tagr[0].stat = 0; //both dead, drop it
    }
      }
      //none of the above, keep as it is
    }
    //Done with bad counters.
    if(AddTAGR){
      if(getBOS(&wcs_, 8 ,(char*) "T")) {
    TAGRevents++;
    DTAGR = (clasTAGR_t *)getBank(&wcs_, "TAGR");
    if(DTAGR){
      //----- will try to Add accidentals-----------------
      //if there is just one photon in the data we will do nothing
      if(DTAGR->bank.nrow >1) {
        if(MTAGR =  (tagr_t *)malloc(sizeof(tagr_t))) {
          if(MTAGR = (tagr_t *)memcpy(MTAGR,
                      &TAGR->tagr[0],
                      sizeof(tagr_t))) {
        //now we can drop original TAGR bank and recreate it
        dropAllBanks(&bcs_, (char*)  "TAGR");
        if(TAGR = (clasTAGR_t *)makeBank(&bcs_,
                         (char*) "TAGR",
                         0,
                         sizeof(tagr_t)/4,
                         DTAGR->bank.nrow)) {
          memcpy(&TAGR->tagr[0], MTAGR, sizeof(tagr_t));
          free(MTAGR); //free this temporary storage
          k=1; //Fill new TAGR bank
          for(i=0; i< DTAGR->bank.nrow; i++) {
            if(DTAGR->tagr[i].stat-GOOD_PHOTON != 15 &&
               k < DTAGR->bank.nrow) {
              TAGR->tagr[k] = DTAGR->tagr[i];
              k++;
            }
          }
          // now update timing and modify status if we
          // happen to have the same T-counter hit
          // for MC and "accidental" photon
          if(TAGR->tagr[0].stat == 7) TAGR->tagr[0].stat = 15;
          //do not touch tagr[0], which is from MC
          for(i=1; i < TAGR->bank.nrow; i++) {
            TAGR->tagr[i].tpho -= tpho_offset;
            TAGR->tagr[i].ttag -= tpho_offset;
            if(TAGR->tagr[i].t_id == TAGR->tagr[0].t_id) {
              //check timing and drop hit that comes later
              if(TAGR->tagr[0].t_id <= TAGR->tagr[i].t_id)
            TAGR->tagr[i].stat += BAD_PHOTON;
              else{
            TAGR->tagr[0].stat += BAD_PHOTON;
               locMCPhotonDroppedFlag = 1;
                }
            }
          }
        }
        else {
          fprintf(stderr,
              "ProcessTAGR -F- "
              "could not make TAGR bank\n");
          exit(1);
        }
          }
          else {
        fprintf(stderr,
            "ProcessTAGR -F- \n"
            "could not memcpy to MTAGR\n");
        exit(1);
          }
        }
        else {
          fprintf(stderr,
              "ProcessTAGR -F- \n"
              "could not malloc for MTAGR\n");
          exit(1);
        }
      } //we do this only if we have more than 1 hit in DTAGR
      //-----------------------------------------------------
    }
      }
      else {
    fprintf(stderr,
        "*****************************************\n"
        "ProcessTAGR -W- "
        "Read %d TAGR events. Cannot read more\n"
        "Will continue anyways, but without adding more accidentals\n"
        "*****************************************\n\n",
        TAGRevents);
    AddTAGR = 0; //no more events left, reset this flag
      }
      dropAllBanks(&wcs_, (char*) "T");
      cleanBanks(&wcs_);
    }
    if((TAGRSync == 1) && (locMCPhotonDroppedFlag == 0)){ //only shift MC photon time
      int locInfoFoundFlag = 0;
      float locVertexZ = 0.0;
      if(MCTK = (clasMCTK_t *)getBank(&bcs_, (char*)"MCTK")){
        for(i = 0; i < MCTK->bank.nrow; i++){
          if((MCTK->mctk[i].id == 22) && (MCTK->mctk[i].end_vtx == 1)){
            if(MCVX = (clasMCVX_t *)getBank(&bcs_, (char*)"MCVX")){
              locVertexZ = MCVX->mcvx[i].z;
              locInfoFoundFlag = 1;
            }
          }
          if(locInfoFoundFlag == 1)
            break;
        }
      }
      if(locInfoFoundFlag == 0){ //try part bank
        if(!(PART = (clasPART_t*)getGroup(&bcs_, "PART", 0)))
          PART = (clasPART_t*)getGroup(&bcs_, "PART", 1);
        if(PART != NULL){
          if(PART->bank.nrow > 0){
            locVertexZ = PART->part[0].vert.z;
            locInfoFoundFlag = 1;
          }
        }
      }
      if(locInfoFoundFlag == 1){
        float locOrigTPho = TAGR->tagr[0].tpho;
        TAGR->tagr[0].tpho -= (locVertexZ - gppTargetCenter[2])/29.9792458;
        TAGR->tagr[0].ttag -= (locVertexZ - gppTargetCenter[2])/29.9792458;
        hf2(603,locVertexZ, TAGR->tagr[0].tpho - locOrigTPho,1.);
      }
    }

    //fill TAGR histograms
    for(i=0; i < TAGR->bank.nrow; i++) {
      if(TAGR->tagr[i].stat == 7 || TAGR->tagr[i].stat == 15) {
    hf1(600,TAGR->tagr[i].tpho,1.);
    if(i != 0) hf1(601,TAGR->tagr[i].tpho,1.);
    hf1(602,TAGR->tagr[i].e_id+0.5,1.);
      }

    }

  }
  return(0);
}
//end ProcessTAGR----------------------------------------------


// Trigger Simulation STUFF ---------------------------------------------------------------
int TrigSect(int *retTrig){

  /* Purpose: Return the sector that caused the trigger.
     Inputs:  CC and EC banks
     Outputs: Number of the sector that caused the trigger
     Author:  Richard A Thompson 97.12

     Revised: Simeon McAleer
     Florida State University
     January 19, 1999

     This routine returns the sector that caused the trigger based on the possible following criteria:

     1)  The sector with the highest EC summed energy over threshold, and/or the
     highest EC Inner Layer summed energy, and/or a CC hit over threshold.
     2)  Some cc tubes may be masked out.  See CC_ID_CUT in tsim.h.
     3)  EC, EC Inner and CC thresholds may be applied via command line of tsim.

     0 will be returned if there was no sector that satisfied the above criteria

     The CC trigger threshold is applied to groups of eight CC PMTs (which I  call CC trigger bits).
     The CC trigger bits are then ||-ed together.
     The trigger bits are staggered by four tubes so that each tube goes into two trigger bits
     except for the first four tubes which only go into one trigger bit.
     The grouping of CC PMTs into cc trigger bits is done in cc_id2TrigBit.

     The EC trigger threshold is simply applied to the sum of all EC hits in a sector.

  */
  int evntclass = 0;
  int evntnum;
  int RunNo, RunType;
  int TrigPass = 0x0;
  static int LastRunNo = -666;
  static int LastRunType = -99;
  int Sect, TrigSect=0, i;
  int id, adc;
  int CCBit1, CCBit2;
  int CCTrigBitAmpl[6][N_CC_TRIG_BITS];  /* indexed on sector and trigger bit within that sector */

  static float ECPeds[6][EC_LAYERS][EC_ORIENTATION][EC_STRIPS];
  static float CCPedsMean[CC_CHANNELS], CCPedsSigma[CC_CHANNELS];

  char *MapDir, ChMap[100];

  pmtHit_t CCAmpl[6];                  /* the amplitude of the trigger bit with the greatest adc */
  pmtHit_t ECAmpl[6];                  /* the sum of EC hits indexed by sector */
  pmtHit_t ECInnerAmpl[6];             /*  the sum of the EC inner layer hits indexed by sector */
  clasCC0_t *cc = NULL;
  clasEC_t *ec = NULL;
  clasHEAD_t *HEAD = NULL;

  HEAD = (clasHEAD_t *)getBank(&bcs_, "HEAD");
  RunNo = HEAD->head[0].nrun;
  RunType = HEAD->head[0].type;
  evntnum = HEAD->head[0].nevent;
  evntclass = HEAD->head[0].evtclass;

  /* initialize */
  for (Sect=1; Sect<7; Sect++){
    CCAmpl[Sect-1].sec = CCAmpl[Sect-1].adc = 0;
    ECAmpl[Sect-1].sec = ECAmpl[Sect-1].adc = 0;
    ECInnerAmpl[Sect-1].sec = ECInnerAmpl[Sect-1].adc = 0;
    for (i=0; i<N_CC_TRIG_BITS; i++){
      CCTrigBitAmpl[Sect-1][i] = 0;
    }
  }

  /* get the pedestals if the run number has changed or
   * if we've changed between REAL and MC data */
  if( (RunNo != LastRunNo) || (LastRunType*RunType < 0) ){
    int CalibRunNo = RunNo;
    if (RunType < 0) CalibRunNo = 1;
    ec_read_map(CalibRunNo, ECPeds, (char*) "EC_PEDESTALS", 400.0);
    if( (MapDir = (char*)getenv("CLAS_PARMS")) ){
      sprintf(ChMap, "%s/Maps/CC_CALIB.map", MapDir);
    }
    get_cc_pedestals_(ChMap, CalibRunNo, CCPedsMean, CCPedsSigma);
  }
  LastRunNo = RunNo;

  if (cctrig) {

    /* parse the cc hits (from tubes) into the appropriate cc trigger bits */
    for (Sect=1; Sect<7; Sect++){
      if( (cc = (clasCC0_t *)getGroup(&bcs_, "CC  ", Sect)) ){
    for(i=0; i<cc->bank.nrow; i++){
      cc_id2TrigBit(cc->cc0[i].id, &CCBit1, &CCBit2);
      id = cc->cc0[i].id;
      if( (adc=cc->cc0[i].adc) ){
        if(CCBit1>0)
          CCTrigBitAmpl[Sect-1][CCBit1 - 1] += adc - (int)CCPedsMean[cc_index(Sect,id)] ;
        if(CCBit2>0)
          CCTrigBitAmpl[Sect-1][CCBit2 - 1] += adc - (int)CCPedsMean[cc_index(Sect,id)];
      }
    }
      }
    }

    /* fill CCAmpl with the amplitude of the trigger bit with the greatest amplitude*/
    for (Sect=1; Sect<7; Sect++){
      CCAmpl[Sect-1].sec = Sect;       /* this looks weird, but it's so that I can later sort without
                      losing track of sector */
      for(i=min_cc_bit; i<N_CC_TRIG_BITS; i++){
    if(CCTrigBitAmpl[Sect-1][i] > CCAmpl[Sect-1].adc)
      CCAmpl[Sect-1].adc = CCTrigBitAmpl[Sect-1][i];
      }
    }
  }

  if (ectrig || ecinnertrig) {
    /* fill ECAmpl with the summed energy in that sector */
    for (Sect=1; Sect<7; Sect++){
      ECAmpl[Sect-1].sec = Sect;
      if( (ec = (clasEC_t *)getGroup(&bcs_, "EC  ", Sect)) ){
    for(i=0; i< ec->bank.nrow; i++){
      id = ec->ec[i].id;
      adc = ec->ec[i].adc;
      if(adc > 0) {         /* sometimes there are zeros, don't add those because after
                   pedestal subtraction they'll be negative */
        ECAmpl[Sect-1].adc += (int)adc -
          (int)(ECPeds[Sect-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)]);

        /* Repeat summation for EC Inner Calculation */

        if (ec_layer(id) == INNER) {    /* INNER defined in ec.h */
          ECInnerAmpl[Sect-1].adc += (int)adc -
        (int)(ECPeds[Sect-1][ec_layer(id)][ec_orientation(id)][ec_strip(id)]);
        }
      }
    }
      }
    }
  }
  /* figure out the trigger sector */
  /* this will report the sector with the highest EC sum over threshold and a CC hit over
     threshold */
  int tmp_trig = 0;
  *retTrig = 0x0;
  int nrbits = 0;
  int nbits;

  for (i=5; i>-1; i--) {

    nbits = 0;
    TrigPass = 0x0;
    if (ectrig && (float) ECAmpl[i].adc >= EC_thresh) {
      TrigPass |= EC_IN_TRIG;
      nbits++;
    }
    if (ecinnertrig && (float) ECInnerAmpl[i].adc >= ECInner_thresh) {
      TrigPass |= ECINNER_IN_TRIG;
      nbits++;
    }
    if (cctrig && CCAmpl[i].adc >= CC_thresh) {
      TrigPass |= CC_IN_TRIG;
      nbits++;
    }
    if (notb1trig) {
      TrigPass |= CC_BIT1_NOT_IN_TRIG;
      nbits++;
    }

    tmp_trig = *retTrig;
    *retTrig = TrigPass;
    TrigPass &= TrigFlag;

    if(TrigPass == TrigFlag) {
      TrigSect = i + 1;
      return TrigSect;
    }

    // keep the best trigger found so far.
    if (nbits > nrbits) {
      nrbits = nbits; // already updated in retTrig
    } else {
      *retTrig = tmp_trig;  // restore retTrig
    }
  }
  return TrigSect;
}
//PART STUFF-------------------------------------------------------------------
int ProcessPART(int partflag){
  clasPART_t *PART = NULL;
  part_t part[PART_SIZE];
  int GroupNo = 0;
  int npart=0, i;

  if ( (PART = (clasPART_t *)getGroup(&bcs_, "PART", GroupNo)) ){
    for (i=0; i <  PART->bank.nrow; i++) {
      PART->part[i].flags = partflag;
      part[npart] = PART->part[i];
      npart++;
    } // end of for
  } // end of if

  if (npart){
    dropBank(&bcs_,(char*) "PART", GroupNo);
    if (npart >= PART_SIZE) {    /* defensive error trap */
      fprintf(stderr, "make_PART_bank: pART_SIZE needs to be increased. %d \n", npart);
    }
    if ( (PART = (clasPART_t *)makeBank(&bcs_, (char*) "PART",GroupNo,sizeof(part_t)/4, npart)) ){
      for(i=0; i < npart; i++) PART->part[i] = part[i];
    }
  }
  return(1);
}



