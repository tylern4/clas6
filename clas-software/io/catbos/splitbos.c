/*
 * splitbos.c   Split bos files to number of events or size.
 *                   - Maurik Holtrop
 *
 * This is a rewrite in functionality of catbos, i.e. it's main intention
 * is to go the other way, to split files up. However, either code seems 
 * quite capable of both functions.
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <malloc.h>
#include <time.h>
#include <sys/stat.h>
#include <ntypes.h>
#include <bostypes.h>

#define VERSION "3.0"

/* ----------- Function prototypes ---------------- */
void ctrlCHandle(int);
void PrintUsage(char *processName);

/* ----------- CLAS DEFINES         ---------------- */

typedef  struct Arguments {
  int maxFileLength;
  int maxFileNum;
  char outfile[256];
  int Renumber;
  int Debug;
  int Batch;
  int Filter;
  int maxEvtNum;
  int Skip_evt;
  int OutSeq_start;
  int RunNumber;
  int Delete;
  char Command[128];
  } Arg_struct;

void parse_arguments(int *argc,char **argv,Arg_struct *Arg);

#define CTYPE_EVENT 1

/* --------------------------------------------------- */


#define BUFSIZE 700000

int Nevents = 0;
int Tevents = 0;

char *initString = "";

#define MAXFILELEN 2000000

main(int argc,char **argv)
{
  FILE *fp = NULL;

  int i,total_size=0,nFileOut=0;
  int Nevents = 0,ThisNevents = 0, nwrite=0;
  int SkipThisEvent=0;
  int NCounter = 100;
  int BOSInputUnitNo = 1,BOSOutputUnitNo=7;
  int SkipEvents;
  char *argptr;
  char *template,*extension;
  char *OutList = "E";
  char out[100],mess[200];
  char filename[256];
  char outf[256];
  char cmdtmp[156];
  char ret_code;
  int nfile;

  Arg_struct A = {0,0,"\0",0,0,0,0,32000000,0,0,0,0,"\0"};

  clasHEAD_t *HEAD;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  parse_arguments(&argc,argv,&A);
  if(NCounter>A.maxEvtNum)NCounter=A.maxEvtNum;

  nfile = A.OutSeq_start;
  if(A.Debug)printf("Starting\n");
  if ( (A.maxFileLength || A.maxEvtNum < 32000000) && A.outfile)
    sprintf(outf,"%s.%2.2d",A.outfile,nfile);
  else if (A.outfile)
    strcpy(outf,A.outfile);
  else{
    fprintf(stderr,"Please specify the output file with the -o option.\n");
    exit(1);
  }
  
  if(!A.Batch && A.RunNumber)printf("Forcing runnumber to %d for each event\n",A.RunNumber);    

  if(A.Debug)printf("Output file: %s\n",outf);    
  initbos();
  unlink(outf);
  sprintf(out, "OPEN BOSOUTPUT UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=3600", BOSOutputUnitNo,outf);
  if (!fparm_c(out)) {
    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));	 
    exit(1);
  }
  
  if (A.Batch<=1) {
    fprintf(stderr,"Number of\t\tSize of\t\tCurrent\t\tCurrent\n");
    fprintf(stderr,"Events\t\t\tFile (KB)\tFile Out\tFile In\n");
    fprintf(stderr,"----------\t\t------------\t-----------\t------------\n\n");
  }
  
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if(A.Debug)printf("Next Arg: '%s' \n",argptr);
    if (*argptr != '-') { 
      sprintf(filename,"%s",argptr);
      sprintf(mess,"OPEN BOSINPUT UNIT=%d FILE=\"%s\" READ", BOSInputUnitNo,argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      
      if(A.Skip_evt){
	SkipEvents=0;
	while (getBOS(&bcs_,BOSInputUnitNo,"E") && SkipEvents < A.Skip_evt ) {
	  if(!A.Batch && SkipEvents%1000==0){
	    if( (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD")) !=NULL){
	      fprintf(stderr,"Now at event %10d \r",HEAD->head->nevent);
	      fflush(stderr);
	    }
	  }
	  SkipEvents++;
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);		    
	}
	if(!A.Batch){
	  printf("\nSkipped %d Events.\n",SkipEvents);
	}
      }

      while (getBOS(&bcs_,BOSInputUnitNo,"E")) {
	Nevents++;
	ThisNevents++;
	SkipThisEvent=0;
	if (!A.Batch) {
	  if (Nevents % NCounter == 0) {
	    fprintf(stderr,"%8d\t\t%4d\t\t%s\t%s\r",Nevents,total_size,outf,filename);
	    fflush(stderr);
	  }
	}

	if( (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD")) !=NULL){
	  if(A.Renumber)HEAD->head->nevent=Nevents;  /* We need to resequence the event numbers. */
	  if(A.RunNumber)HEAD->head->nrun=A.RunNumber;

	  if(A.Filter && HEAD->head->nevent==0){ /* Skip this event */
	    SkipThisEvent=1;
	  }
	}else{
	  if(A.Filter) SkipThisEvent=1;
	}

	if (A.outfile && !SkipThisEvent ) {
	  putBOS(&bcs_, BOSOutputUnitNo, OutList);
	  nwrite++;
	}

	total_size = fileLength("BOSOUTPUT");
	dropAllBanks(&bcs_,"E");
	cleanBanks(&bcs_);		    
	if( (ThisNevents > A.maxEvtNum) ||
	    (A.maxFileLength && (total_size > A.maxFileLength))) {

	  /* We reached the maximum desired size for the output, close file and open a new one.*/

	  if (A.Batch<=1) {
	      fprintf(stderr,"%8d\t\t%4d\t\t%s\t%s\n",Nevents,total_size,outf,filename);
	      fflush(stderr);	    
	  }

	  sprintf(mess,"CLOSE BOSOUTPUT UNIT=7");
	  fparm_c(mess);
	  nfile++;
	  if(A.maxFileNum && (nfile-A.OutSeq_start)>A.maxFileNum-1){
	    break;
	  }

	  /* Execute a command on the output file. This is tremendoesly useful in batch processing.
	   * For instance the command can be a script that submits a farm job for each file that 
	   * is split off from the 'master input'. Or, after the many input files are concatenated
	   * a script will copy the output to tape. If you BACKGROUND (use &) the command, then the
	   * splitbos program will not wait for it to finish execution.
	   */

	  if(A.Command[0]!=0){
	    strcpy(cmdtmp,A.Command);
	    strcat(cmdtmp," ");
	    strcat(cmdtmp,outf);
	    if(A.Debug)printf("Executing Command: %s \n\n",cmdtmp);
	    ret_code = system(cmdtmp);
	  }

	  /* Open new output file in sequence. */

	  sprintf(outf,"%s.%2.2d",A.outfile,nfile);
	  if(A.Debug)fprintf(stderr,"Output file: %s\n",A.outfile);
	  unlink(outf);
	  sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outf);
	  if (!fparm_c(out)) {
	    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));	 
	    exit(1);
	  }
	  ThisNevents=0;
	  if(A.Renumber==2)Nevents=0;
	  fprintf(stderr,"\n");
	}
      }

      /* Close the input file to get ready for the next one. */

      sprintf(mess,"CLOSE BOSINPUT UNIT=%d ",BOSInputUnitNo, argptr); 
      fparm_c(mess);
      if (A.Batch<=1) {
	  fprintf(stderr,"%8d\t\t%4d\t\t%s\t%s",Nevents,total_size,outf,filename);
      }
/*
 * Delete the input file, if asked for. This is obviously a dangerous operation, since we can not be
 * 100% sure of the output file's integrity (code may crash later and leave a bad BOS file. However,
 * when short of disk space, this is sometimes the only way to copy your many input files in to a few 
 * output files.
 */

      if (A.Delete == 1){
	unlink(filename);
	if(A.Batch<=1)fprintf(stderr," DELETED\n");
      }else if(A.Batch<=1)fprintf(stderr,"\n");

      fflush(stderr);	
    }
  }

  /* close the input and output files */

  sprintf(mess,"CLOSE BOSOUTPUT UNIT=%d ",BOSOutputUnitNo, A.outfile); 
  fparm_c(mess);
  if (A.Batch<2) {
    fprintf(stderr,"\n%8d\t\t%4d\t\t%s\t%s\n",Nevents,total_size,outf,filename);
    fflush(stderr);
  }

  if(A.Command[0]!=0){
    strcpy(cmdtmp,A.Command);
    strcat(cmdtmp," ");
    strcat(cmdtmp,outf);
    if(A.Debug)printf("Executing Command: %s \n\n",cmdtmp);
    ret_code = system(cmdtmp);
  }

}
  

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  exit(1);
}


void parse_arguments(int *argc,char **argv,Arg_struct *Arg)
{ 
/* Read the command line and set all the switches etc. */
/* This routine uses a standard loop that removes all command line
 * arguments that start with a -, and sets the appropriate flag.
 * All flags are globals so that all routines can "listen in".
 * The remaining arguments are all the input files.
 */

 /* Macro to remove 1 item from argument list. */
#define REMOVE_ONE {(*argc)--;for(j=i;j<(*argc);j++)argv[j]=argv[j+1];i--;}
#define I_PLUS_PLUS if((i+1)<(*argc)){i++;}else{break;}

  int i,j,skip;
  
  if( *argc<2){
    *argc=2;
    argv[1]="-h";
  }

  for(i=1;i<(*argc);i++)
    {
      if(argv[i][0]=='-')
	{
	  if(strcmp(argv[i],"-quiet")==0 || strcmp(argv[i],"-q"  )==0 || strcmp(argv[i],"-B"  )==0)
	    {
	      Arg->Batch=2;	      
	    }
	  else if(strcmp(argv[i],"-batch")==0 )
	    {
	      Arg->Batch=1;	      
	    }
	  else if(strcmp(argv[i],"-delete")==0 )
	    {
	      Arg->Delete=1;	      
	    }
	  else if(strcmp(argv[i],"-filter")==0 )
	    {
	      Arg->Filter=1;	      
	    }
	  else if(strcmp(argv[i],"-debug")==0 || strcmp(argv[i],"-d")==0)
	    {
	      Arg->Debug++;
	    }
	  else if(strcmp(argv[i],"-renumber")==0 || strcmp(argv[i],"-R")==0)
	    {
	      Arg->Renumber=1;
	    }
	  else if(strcmp(argv[i],"-renumeach")==0 || strcmp(argv[i],"-r")==0)
	    {
	      Arg->Renumber=1;
	    }
	  else if(strcmp(argv[i],"-skip")==0 )
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%d",&Arg->Skip_evt);
	      if(!Arg->Batch)printf("Skipping %d events of each input file.\n",Arg->Skip_evt);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-seq")==0 )
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%d",&Arg->OutSeq_start);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-maxnum")==0 || strcmp(argv[i],"-n")==0)
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%d",&Arg->maxEvtNum);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-maxlen")==0 || strcmp(argv[i],"-m")==0)
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%d",&Arg->maxFileLength);
	      REMOVE_ONE;
	    }

	  else if(strcmp(argv[i],"-maxfile")==0 || strcmp(argv[i],"-M")==0)
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%d",&Arg->maxFileNum);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-outfile")==0 || strcmp(argv[i],"-o")==0)
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%s",Arg->outfile);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-runnum")==0 )
	    {
	      I_PLUS_PLUS; 
	      sscanf(argv[i],"%d",&Arg->RunNumber);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-command")==0 )
	    {
	      I_PLUS_PLUS; 
	      strcpy(Arg->Command,argv[i]);
	      REMOVE_ONE;
	    }
	  else if(strcmp(argv[i],"-help")==0|| strcmp(argv[i],"-h")==0)
	    {
	      fprintf(stderr,"\n***** splitbos %s *****\n\n",VERSION);
	      fprintf(stderr,"Usage: %s -o <fileoutputstub> -n # file1 file2 ... \n\n",argv[0]);
	      fprintf(stderr,"\tSplitbos splits the <file1> <file2> ... chain into N files\n");
	      fprintf(stderr,"\teach with # events in them. If # is large files are concatenated.\n\n");
	      fprintf(stderr,"\t-outfile   -o f\tOutput stub name of final file name\n");
	      fprintf(stderr,"\t               \tthe file sequence # is appended.\n");
	      fprintf(stderr,"\t-maxnum    -n #\tSet maximum number of events in outfile.\n");
	      fprintf(stderr,"\t-maxlen    -m #\tSet maximum file length to # kbytes, instead or\n");
	      fprintf(stderr,"\t               \tin addition to maximim number of events.\n");
	      fprintf(stderr,"\t-maxfile   -M #\tSet maximum number of output files.\n");
	      fprintf(stderr,"\t-renumber  -R\tRenumber the events in HEAD bank so that the output\n");
	      fprintf(stderr,"\t             \thas consecutive event numbers FOR ALL OUTPUT FILES.\n");
	      fprintf(stderr,"\t-renumeach -r\tRenumber the events in HEAD bank so that the output\n");
	      fprintf(stderr,"\t             \thas consecutive event numbers FOR EACH OUTPUT FILE.\n");
	      fprintf(stderr,"\t-skip   #    \tSkip the first # events.\n");
	      fprintf(stderr,"\t-seq    #    \tStart the output sequence with # .\n");
	      fprintf(stderr,"\t-runnum #    \tFORCE the runnumber to # for each event. DANGEROUS.\n");
	      fprintf(stderr,"\t-delete      \tDELETE the input file as soon as it's copied. DANGEROUS.\n");
	      fprintf(stderr,"\t-debug     -d\tUp the debug verbosity by one.\n");
	      fprintf(stderr,"\t-batch     -q\tRun batchmode= don't count, but do report the files.\n");
	      fprintf(stderr,"\t-command 's' \tAfer a split off file is closed, launch the command.\n");
	      fprintf(stderr,"\t             \tNote that the command is appended by the split file.\n");
	      fprintf(stderr,"\t             \tNote also that this is though the system() call and\n");
	      fprintf(stderr,"\t             \tand thus this program will wait until execution is finished.\n");
	      fprintf(stderr,"\t-filter      \tFilter out events that have event number =0, this removes\n");
	      fprintf(stderr,"\t             \tsome junk events that can cause recsis to abort early.\n");
	      fprintf(stderr,"\t-quiet     -q\tRun quietly.\n");
	      fprintf(stderr,"\t-help      -h\tPrint this message.\n\n");
	      fprintf(stderr,"Suggestions? Email me at maurik.holtrop@unh.edu.\n\n");
	      exit(1);
  	    }
	  else
	    {
	      fprintf(stderr,"\nI did not understand the option : %s\n\n",argv[i]);
	      argv[1]="-help";
	      *argc=2;
	      parse_arguments(argc,argv,Arg); /* Recurse to get help screen. */
	      exit(1);
	    }
	  /* KILL the option from list */
	  REMOVE_ONE;
	}
    }
  
  return;
}
