/*
 * countbos.c   Display contents of Bos Banks
 *              - J. J. Manak
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef Darwin
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif
#include <time.h>
#include <sys/stat.h>
/*#include <disData.h>
#include <tapeData.h>
#include <dataIO.h>*/
#include <ntypes.h>
#include <bostypes.h>


/* ----------- Function prototypes ---------------- */
/* int fpinit(char *filename); 
void *getGroup(char *name, int j);
*/
int ProcessEvent();
void ctrlCHandle(int);
void PrintUsage(char *processName);
void dump(char *x,int nchar);

/* ----------- CLAS DEFINES         ---------------- */

#define CTYPE_EVENT 1


/* -------------------BOS -------------------------- */

/*#define NDIM 20000

typedef struct bosbank {
  int ipad[5];
  int iw[NDIM];
} BOSbank;

BOSbank bcs_;
*/

#ifdef Darwin
BOSbank wcs_;
#endif

/* --------------------------------------------------- */

#define BUFSIZE 700000

int Nevents = 0;
int Tevents = 0;

char *initString = "";


void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s file1 [file2] etc....\n",processName);
  fprintf(stderr,"\t-counts the number of events in a BOS format file\n\n");
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i;
  char *argptr;
  int Nevents = 0;
  int Ntotal = 0;
  int UnitNo = 1;
  char mess[200];
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  if(argc == 1) PrintUsage(argv[0]);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  
  initbos();
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') { 
      sprintf(mess,"OPEN BOSINPUT UNIT=%d FILE=\"%s\" READ", UnitNo,argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      /*now start counting*/
      while (getBOS(&bcs_,UnitNo,"E") ) {
	Nevents++;
	Ntotal++;
	if (Nevents % 100 == 0) {
	  fprintf(stderr,"%d\r",Nevents);
	  fflush(stderr);
	}
	ProcessEvent();
	dropAllBanks(&bcs_,"E");
	cleanBanks(&bcs_);
      }
      fprintf(stdout,"%d BOS events in file: %s\n",Nevents, &argv[i][0]);
      sprintf(mess,"CLOSE BOSINPUT UNIT=%d",UnitNo);
      fparm_c(mess);
      Nevents = 0;

    }
      fprintf(stdout,"Total: %d BOS events\n",Ntotal);    
  }
  
}

/*not used */
int fpinit(char *filename) 
{
  int iret, iflg = 1;
  int nbcs = NDIM;
  char mess[1024],errmess[1024];

  bos_(bcs_.iw,&nbcs);
  sprintf(mess," OPEN BOSINPUT FILE=\"%s\"  READ %s",filename,initString);
  fparm_(mess,strlen(mess));
  fermes_(errmess,&iflg,strlen(errmess));
  fprintf(stderr,"string:%s, %d\n", errmess, iflg);
  return(1);
}

int ProcessEvent()
{
  /*do nothing*/
  return(1);
}

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  exit(1);
}


/* end file */







