#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <map_manager.h>
#include <status.h>
#include "pdu.h"

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int   quest_[100];
extern BOSbank bcs_;

FILE *OutFile = NULL;            /* output log file */
FILE *WireFile = NULL;           /* output file containing list of pathological
				    wires */
int   max = 0;                                    /* number of events to process     */

/* ------------------------------------------------------------------------ */
main(int argc,char **argv)
{
  FILE *fp = NULL;
  int   i;
  /*  char *argptr; Commented out for basename*/
  char *InFilePtr;                                  /* the input data file             */
  char  outfile[30];
  char  FileName[30];
  int   ReadOnlyMode = 0;
  int   icycle;                                     /* some hbook thing                */
  int   IFail;                                      /* general use routine return      */
  int   Sector, Region, Layer, RealLayer, Wire;
  int   OutHits[SECTORS][LAYERS-2][WIRES];            /* hit spectra for pulsing out     */
  int   WireFlag[SECTORS][LAYERS][WIRES];           /* the wire status                 */
  int   PreviousWireFlag[SECTORS][LAYERS][WIRES];   /* the wire status for the last run */
  int   HistID;
  int   HistWeight;
  int   NumHot[REGIONS][SECTORS], 
        NumDead[REGIONS][SECTORS];                /* number of hot and dead wires found */  
  int   runno = 0;
  int   RunNo = 0;
  char  Message[100];
  /* char  mess[100]; Commented out for basename */
  char  chRunno[6];
  int   BannR;
  int   Nevents = 0;
  int   batch = 0;
  int   base = 0;
  clasHEAD_t *header = NULL;

  char mess[100],
       * argptr,
       * basename;

  int write =0;               /* added on 12/05/2000 by Maurizio Ungaro: force writing in the map */
  int Map_Index; 
  int status[NSECTOR *  CHANNELS];
  char *MapDir, ChMap[100];

  /* control signal handling */
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  
  /* first process the command line arguments */
  if (argc < 2) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'n':
	max = atoi(++argptr);
	break;
      case 'R':
	ReadOnlyMode = 1;
	fprintf(stderr, "Only read map mode\n");
	runno = atoi(++argptr);
	break;
      case 'b':
	batch = 1;
	fprintf(stderr, "pdu: Running in batch mode. No count updates.\n");
	break;
      case 'W':
	write = 1;
	RunNo = atoi(++argptr);
	fprintf(stderr, "pdu: Forcing writing in the map for run %d\n",RunNo);
	break;
      case 'o':
	base = 1;
	if (*(++argptr)) basename = argptr;
	fprintf(stderr, "pdu: Using basename %s for out files\n", basename);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
    else InFilePtr = argv[i];
  }
  
  /*Welcome();*/
  initbos();

  /* open files */
  if(!ReadOnlyMode){
    sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", InFilePtr);
    if (!fparm_c(mess)) {
      printf("pdu : Unable to open file \'%s\': %s\n\n",InFilePtr,strerror(errno));
      exit(1);
    }
    runno = get_run_number(); 
  }

  if (base) {
    sprintf(outfile, "pdu_%s.hbk", basename);
    sprintf(FileName, "pdu_%s.out", basename);
  } else {
    sprintf(outfile, "pdu_%i.hbk", runno);
    sprintf(FileName, "pdu_%i.out", runno);
  }
  if((OutFile=fopen(FileName, "w"))==NULL) {
    puts("Jeepers, couldn't open output file pdu.out\n");
    exit(1);
  }

  if (base) {
    sprintf(FileName, "pdu_wire_%s", basename);
  } else {
    sprintf(FileName, "pdu_wire_%i",runno);
  }
  if((WireFile=fopen(FileName, "w"))==NULL) {
    puts("Jeepers, couldn't open output file pdu_wire\n");
    exit(1);
  }

  /* initializations */
  sprintf(Message,"pdu: initializing histograms");
  PrntMessage(Message,OutFile,1);
  hini(outfile);

  /* initialize wire status array */
  for (Sector=0; Sector<6; Sector++){
    for (Layer = 0; Layer<LAYERS; Layer++){
      for (Wire = 0; Wire<WIRES; Wire++){
	WireFlag[Sector][Layer][Wire] = DC_NOT_SET;
      }
    }
    for (Region=0; Region<3; Region++){
      NumDead[Region][Sector] = 0;
      NumHot[Region][Sector]  = 0;
    }
  }
  /* get the wire status array for this run from the map */
  ReadMapStatus(PreviousWireFlag, runno);
  
  if(ReadOnlyMode)      /* don't analyze any data, just read from the map */
    ReadMapStatus(WireFlag, runno);
  else {
    /* initialize  OutHits */
    /* NB: OutHits is indexed over the 34 real layers, not including the two R1 phantom layers */
    for (Sector=0; Sector<6; Sector++){
      for (Layer=0; Layer<34; Layer ++){
	for (Wire=0; Wire<WIRES; Wire++){
	  OutHits[Sector][Layer][Wire] = 0;
	}
      }
    }
    
    /* count the hits on each wire */
    sprintf(Message,"pdu: taking hit spectra");
    PrntMessage(Message,OutFile,1);
    while ((max ? Nevents < max : 1) && (getBOS(&bcs_,1,"E"))) {
      Nevents++;
      /* give the user something to look at */
      if ((Nevents % 1000 == 0) && !(batch)){
	fprintf(stderr,"%d\r",Nevents);
	fflush(stderr);
      }
      CountHits(OutHits);
      dropAllBanks(&bcs_,"E");
      cleanBanks(&bcs_);
    }                                           /* end of event loop */
    sprintf(Message,"\n%d events analyzed from file %s\n",Nevents, InFilePtr);
    PrntMessage(Message,OutFile,0);
    fflush(stderr);

    /* set wire status array */
    sprintf(Message,"pdu: flagging bad wires");
    PrntMessage(Message,OutFile,1);
    FlagBadWires(OutHits, WireFlag);
  }

  /* Write final wire status results to the wire file */
  for (Sector=0; Sector<6; Sector++){
    for (Wire = 0; Wire < WIRES; Wire++){
      for (Layer=0; Layer<LAYERS; Layer++){
	/* only write out pathologies */
	if((WireFlag[Sector][Layer][Wire] != DC_GOOD_WIRE) && 
	   (WireFlag[Sector][Layer][Wire] != DC_DNE)){
	  fprintf(WireFile, "%i %i %i %i\n", Sector+1, Layer+1, Wire+1, 
		  WireFlag[Sector][Layer][Wire]);
          Map_Index = (Sector) * CHANNELS + (Layer)*WIRES_PER_LAYER + Wire;
          status[Map_Index]=WireFlag[Sector][Layer][Wire];
         
	}
      }
    }
    if(write==1) /* Write to map */
    {
     char SECT[100];
     sprintf(SECT, "sector%1d", Sector+1);
     MapDir = (char *)getenv("CLAS_PARMS");
     sprintf(ChMap,"%s/Maps/DC_STATUS.map",MapDir);
     map_rem_arr(ChMap,SECT, "status",RunNo);
     map_put_int(ChMap, SECT, "status", CHANNELS, &status[Sector * CHANNELS], RunNo);
    }

  }
	  
  /* make a histogram */
  for (Sector=1; Sector<7; Sector++){
    for (RealLayer=1; RealLayer<35; RealLayer++){
      Layer = (RealLayer>4) ? RealLayer + 2 : RealLayer;
      for (Wire=1; Wire<=WIRES; Wire++){
        HistID = 200 + 10*Sector;
        hfill(HistID, (float)Wire, (float)Layer, OutHits[Sector-1][RealLayer-1][Wire-1]);
      }
    }
  }
  for (Sector=0; Sector<6; Sector++){
    HistID = 700 + 10*(Sector+1);
    for (Wire = 0; Wire < WIRES; Wire++){
      for (Layer=0; Layer<LAYERS; Layer++){
	Region = (int)((Layer)/12) + 1;
	switch (WireFlag[Sector][Layer][Wire]) {
	case DC_GOOD_WIRE:   /* fine */
	  HistWeight = 1;
	  break;
	case DC_DEAD_WIRE:   /* dead */
	  NumDead[Region-1][Sector]++;
	  HistWeight = 0;
	  break;
	case DC_HOT_WIRE:   /* hot */
	  NumHot[Region-1][Sector]++;
	  HistWeight = 2;
	  break;
	case DC_DNE:         /* no wire here */
	  HistWeight = 3;
	  break;
	case DC_NOT_SET:     /* oops, never got set */
	  HistWeight = -1;
	  break;
	default:   /* this should never happen */
	  sprintf(Message,"pdu ERROR - sector %d layer %d wire %d was not flagged.\n",
		  Sector+1, Layer+1, Wire+1);
	  PrntMessage(Message,OutFile,0);
	  HistWeight = 3;
	  break;
	}
	hfill(HistID, (float)(Wire+1), (float)(Layer+1), HistWeight);
      }
    }
  }

  sprintf(Message,"\npdu: detected pathologies for run %d :", runno);
  PrntMessage(Message,OutFile,0);
   
  /* write to log file */
  sprintf(Message, "\tSector\tRegion\tdead\t%%\thot\t%%");
  PrntMessage(Message, OutFile, 0);
  for (Sector=1; Sector<7; Sector++){
    int SumWires[3] = {1296, 2070, 2304};
    for (Region=1; Region<4; Region++){
      sprintf(Message,"\t%d\t%d\t%d\t%4.2f\t%d\t%4.2f", Sector, Region, 
	      NumDead[Region-1][Sector-1], 100.*(float)NumDead[Region-1][Sector-1]/SumWires[Region-1],  
	      NumHot[Region-1][Sector-1], 100.*(float)NumHot[Region-1][Sector-1]/SumWires[Region-1]);
      PrntMessage(Message,OutFile,0);
    }
  }
  if(!ReadOnlyMode)
    ReportChange(WireFlag, PreviousWireFlag, OutFile);
  
  if(fclose(OutFile)!=0) fprintf(stderr, "Holy cow, couldn't close output file\n");
  if(fclose(WireFile)!=0) fprintf(stderr,"Holy cow, couldn't close output file\n");
  
  hrout(0,icycle," ");
  /*hldir_(" ", " ", 1L, 1L);*/ 
  hrend_("esr", 3L);
}

/* ------------------------------------------------------------------------ */
void PrintUsage(char *processName)
{
  fprintf(stderr,"\nUsage: %s -n[hor] file1 [file2] ...\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-R[runno]\tOnly read map mode.\n");
  fprintf(stderr,"\t-n[#]    \t# number of events to process\n");
  fprintf(stderr,"\t-b       \tRun in batch mode. No count updates\n");
  fprintf(stderr,"\t-o       \tChange basename of outfiles. Ex: pdu_wire_basename.\n");
  fprintf(stderr,"\t-W[runno]\tForce writing in the map for runno.\n");
  fprintf(stderr,"\t-h       \tPrint this message.\n"); 

  exit(0);
}

/* ------------------------------------------------------------------------ */
int get_run_number(){
  /* This wastes the first event on getting the run number.  What's a better way? */
  clasHEAD_t * header;
  int Ret = 0;
  
  if((getBOS(&bcs_,1,"E") != -1) && (header = getBank(&bcs_, "HEAD"))) 
    Ret =  header->head[0].nrun;
  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);
    
  return Ret;
}
/* ------------------------------------------------------------------------ */
void ctrlCHandle(int x){
  extern int max;

  fprintf(stderr, "Caught control signal %d\n", x);
  max = 1;
  
}

/* ------------------------------------------------------------------------ */
int Welcome(){
  printf("           ______________________________________\n");
  printf("          |               P  D  U                |\n");
  printf("          |    a Pathology Diagnostics Utility   |\n");
  printf("          |     for the CLAS drift chambers      | \n");
  printf("          |______________________________________|\n\n");
  fprintf(OutFile,"           ______________________________________\n");
  fprintf(OutFile,"          |               P  D  U                |\n");
  fprintf(OutFile,"          |    a Pathology Diagnostics Utility   |\n");
  fprintf(OutFile,"          |     for the CLAS drift chambers      | \n");
  fprintf(OutFile,"          |______________________________________|\n\n");

  return 0;
}

/* ------------------------------------------------------------------------ */
void PrntMessage(char *Mess, FILE *fp, int BannR) {
  char Banner[] = "***************************************";

  if(BannR) fprintf(stderr, "%s\n", Banner);
  fprintf(stderr, "%s\n", Mess);
  if(BannR) fprintf(stderr, "%s\n", Banner);
  if(BannR) fprintf(fp, "%s\n", Banner);
  fprintf(fp, "%s\n", Mess);
  if(BannR) fprintf(fp, "%s\n", Banner);

return;
}
/* ------------------------------------------------------------------------ */
int ReadMapStatus( int WireFlag[SECTORS][LAYERS][WIRES], int runno ){
  /* fills PreviousWireFlag with the flags for run runno-1 */

  int   Sector, Layer, Wire;
  char  ChSector[50];
  int   Index;
  char *MapDir, ChMap[100];
  int   Status[SECTORS * CHANNELS]; 
  int   FirstTime;     /* Argument need for get_map.  What the hell is this? */

  /* inititialize */
  MapDir = (char *)getenv("CLAS_PARMS");
  sprintf(ChMap,"%s/Maps/DC_STATUS.map",MapDir);
  for(Sector=0; Sector<6; Sector++){
    sprintf(ChSector, "sector%1d", Sector+1);
    map_get_int(ChMap, ChSector, "status", CHANNELS, &Status[(Sector) * CHANNELS], runno, 
		&FirstTime);
    for(Index = (Sector)*CHANNELS; Index < (Sector+1)*CHANNELS; Index++){
      Layer = (Index - (Sector)*CHANNELS)/WIRES;
      Wire = Index - (Sector)*CHANNELS - (Layer)*WIRES;
      WireFlag[Sector][Layer][Wire] = Status[Index] ;	
    }
  }
    return 1;
}
/* ------------------------------------------------------------------------ */
int ReportChange(int WireFlag[SECTORS][LAYERS][WIRES], 
		 int PreviousWireFlag[SECTORS][LAYERS][WIRES], FILE *fp){
  /* reports on the changes in status between the two Flag arrays */
  /* summary to stderr and a histogram */
  
  int   Num2Good[SECTORS][REGIONS];
  int   Num2Dead[SECTORS][REGIONS];
  int   Num2Hot[SECTORS][REGIONS];
  int   Sector, Layer, Region, Wire;
  int   HistID, HistWeight;
  char  Message[100];

  /* initialize */
  memset((void *)Num2Good, 0, SECTORS*REGIONS*sizeof(int));
  memset((void *)Num2Dead, 0, SECTORS*REGIONS*sizeof(int));
  memset((void *)Num2Hot, 0, SECTORS*REGIONS*sizeof(int));
  
  for (Sector = 0; Sector < SECTORS; Sector++){
    for(Layer = 0; Layer < LAYERS; Layer++){
      Region = Layer/12;
      HistID = 300 + (Sector+1)*10;
      for(Wire = 0; Wire < WIRES; Wire++){
	int HistWeight = 0;
	/* if the status has changed, report it */
	if(WireFlag[Sector][Layer][Wire] - PreviousWireFlag[Sector][Layer][Wire]){
	  switch (WireFlag[Sector][Layer][Wire]) {
	  case DC_GOOD_WIRE:   
	    Num2Dead[Sector][Region]++;
	    HistWeight = 2;
	    break;
	  case DC_DEAD_WIRE:
	    Num2Good[Sector][Region]++;
	    HistWeight = 1;
	    break;
	  case DC_HOT_WIRE:
	    Num2Hot[Sector][Region]++;
	    HistWeight = 3;
	    break;
	  }
	}
	/* does not exist */
	if(WireFlag[Sector][Layer][Wire] == DC_DNE)
	  HistWeight = 4;

	hf2(HistID, (float)(Wire + 1), (float)(Layer+1), HistWeight);
      }    /* end wire loop */
    }    /* end layer loop */
  }    /* end sector loop */
  sprintf(Message, "\nReportChange: changed pathologies:");
  PrntMessage(Message,OutFile,0);
  sprintf(Message, "\tSector\tRegion\tdead\tgood\thot");
  PrntMessage(Message,OutFile,0);
  for(Region = 0; Region < REGIONS; Region++){
    for(Sector= 0; Sector < SECTORS; Sector++){
      sprintf(Message, "\t%i\t%i\t%i\t%i\t%i", Sector+1, Region+1, Num2Dead[Sector][Region], 
	  Num2Good[Sector][Region], Num2Hot[Sector][Region]);
      PrntMessage(Message,OutFile,0);
    }
  }
  fprintf(stderr, "\n--------------------------------------------------------------------\n");
  return 1;
}

