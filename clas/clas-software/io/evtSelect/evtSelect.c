/*
 Name: evtSelect
 Usage: evtSelect < selectFile sourceFile > outFile
 Purpose:  from the source file, output only selected events
 Notes: selectFile in format (line by line): Run# Event#
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_; 
/* ----------- Function prototypes ---------------- */

int ProcessEvent(int,int,int);
void ctrlCHandle(int);
void PrintUsage(char *processName);

/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-M] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-M[#]\tPrint out only # number of events\n\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}

  get_line(s,lim,fp)
FILE *fp;
char s[];
int lim;
/* 
    FUNCTION:
        This routine reads a line from a file pointer.

    INPUT_PARAMETERS:
        lim - int
            the limit on the length of the line to get

        fp - open file pointer

    OUTPUT_PARAMETERS:
        s - pointer to character array
            the array to fill with the line

    RETURN_VALUE:
        returns the length of the line read from the file pointer
*/
{
        int c,i;
        for(i=0;i<lim-1 && (c=getc(fp)) != EOF && c != '\n';++i)
                s[i]=c;
        if(c == '\n' ){            
                s[i]=c;
                ++i;
        }
        s[i]='\0';
        return(i);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i;
  char *argptr;
  int Nevents = 0;
  int max = 0;
  char mess[200];
  int run,event;
  int more;
  char *outfile = NULL;
  char out[100];
  int verbose = 0;

  clasHEAD_t *HEAD;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'v':
	verbose = 1;
	break;
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'M':
	max = atoi(++argptr);
	break;  
      case 'o':
	outfile =  *(++argptr) ? argptr : "/dev/fd/1";
	fprintf(stderr,"Output file: %s\n",outfile);
	sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outfile);
	if (!fparm_c(out)) {
	  fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));	 
	  exit(1);
	}
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  more =   getInput(&run,&event);
  initbos();
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	while (more && (max ? Nevents < max : 1) && (getBOS(&bcs_,1,"E") != 0)) {
	  Nevents++;
	  if (Nevents % 100 == 0) {
	    if (HEAD = (clasHEAD_t *) getBank(&bcs_,"HEAD")) {
	      fprintf(stderr,"%d\t%d\t%d\r",Nevents,HEAD->head[0].nrun,HEAD->head[0].nevent);
	      fflush(stderr);
	    }
	  }
	  if  ( ProcessEvent(run,event,verbose) ) {
	    fprintf(stderr,"Found event: %d %d\n",run,event);
	    more =  getInput(&run,&event);
	    putBOS(&bcs_, 7, "E");

	  }

	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	}
	fprintf(stderr,"#  of events: %d\n",Nevents); 

      }
    
    }

  }
  /*close file*/
  sprintf(mess,"CLOSE BOSOUTPUT UNIT=7");
  fparm_c(mess);	
  fprintf(stderr,"Total #  of events: %d\n",Nevents); 

}

int ProcessEvent(int run,int event,int verbose)
{
  clasHEAD_t *header;

  if(header = (clasHEAD_t *)getBank(&bcs_, "HEAD")){
    if (verbose)
      fprintf(stderr,"Checking event %d %d\n",header->head[0].nrun,header->head[0].nevent);

    return((header->head[0].nrun == run) && (event == header->head[0].nevent));
  }
  else
    return(0);
} 

int getInput(int *run,int *event)
  {
    char line[1024];
    int ret ;
    char *word;
 
  
  /* read from stdin the next run,event to select */
  if (ret = get_line(line,1024,stdin)) {
    word = strtok(line," ,\n");
    if (ret && word)            
      *run = atoi(word);
    else
      ret = 0;
    if (ret && (word = strtok(NULL," ,\n"))) 
      *event = atoi(word);
    else 
        ret  = 0;
  }
  return(ret);
 
  }
      


void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  exit(1);
}

/* end file */







