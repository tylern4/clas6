/*
 * aligndc.c       Rob Feuerbach 8-Jun-98
 * 
 * DC alignment program. reads in TBLA bank from cooked file,
 * and move the chambers about to best improve the chi2 of the
 * (unchanging) track.
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <kinematics.h>
#include <ntypes.h>
#include <bostypes.h>
#include <utility.h>
#include "nrutil.h"
#include <setjmp.h>
#include "aligndc.h"
#include "dcg_tools.h"


#define XINC 0.2
#define SINCX 0.0032
#define SINCY 0.0032
#define SINCZ 0.0032

#define PRECISION 0.00002

#define NFILES 15

BOSbank bcs_;
BOSbank wcs_;

/* global control variables */

int Sector[7] = {0,0,0,0,0,0,0}; /* pointer to boolean list, from 1 to 6 of
				  * sectors to use.
				  * Sector[0] = multiple sectors
				  * aligned coherently
				  */

int RegMove[3] = {0,-1,0};       /* region(s) to adjust to align and
				  * to calc chi2 from
				  */

int MaxHoles[6] = {1,2,2,2,2,2}; /* Maximum number of "faked" hits permitted
				  * in a given superlayer.
				  */

int ndim;                        /* number of dimension to fit to
				  */

int dim[6] = {1,2,3,0,0,0};      /* array list of dimensions to fit
				  */

int layertype = 1;               /* type of layers used in minimization
				  * (1=AX,2=ST,3=AX+ST)
				  */

int nevent = 0;                  /* number of events to read in
				  * from the file
				  */

int jumpevnt = 0;                /* number of events to skip
				  * before processing nevent's
				  */

int WeightOn = 0;                /* whether to do dynamic weighting by
				  * cos(theta) or not
				  */

float weightmult[3] = {1.,1.,1.}; /* times larger the sigma is than it
				   * should be.
				   */

int debug = 0;                    /* debugging output on
				   */

/* other necessary evil global variables */
char **dimlabel;                /* array of pointers to labels */
track_t *toptrack = NULL;       /* topmost track in the linked list */
char *command;
float xpos[3][18], sdir[3][18]; /* initial reference offsets */
float xt[3],st[3];              /* initial torus offsets */
float maxChi2 = -1;

/* for safe exit on interrupt */
jmp_buf env;

/* local function prototypes */
track_t *getTBLAtoTRACKS(char *fname[], int nfile, int *runno);
float chi2(track_t *track,float *nhits);
float doca(track_t *track,hit_t *hit,float *trsigma);
double evaluate(int dummy, int ndim, float *point);
int doAmoeba(float ftol);
double StartPoint(int sector, int ndim, int trials, float *dxs);
static void signal_handler(int sig);
void init_bos_banks(void);
void amoeba(int sec,float **p, double y[], int ndim, float ftol,
	    double (*funk)(int sec, int ndim, float []),int *nfunk);
float weight(track_t *track);
int cuts(track_t *track);
int filltrack(track_t **track, int evnt);

float get_trsigma(track_t *track, hit_t *hit, vector3_t wpos, vector3_t wdir);
float tracksigma(track_t *track, vector3_t wpos, vector3_t wdir);
void SigmaParam(float *err, int ndim);


void PrintUsage(char *processname)
{
  fprintf(stderr,"\nUsage:\n %s -o<results> [-options] <file>\n",
          processname);
  fprintf(stderr,"\t-o<results>     final fitted file of form used by dc_map\n");
  fprintf(stderr,"\t <file>         cooked file containing B=0 tracks to be used.\n");
  fprintf(stderr,"Options : (default)\n");
  fprintf(stderr,"\t-i<offsets>     offset file of form used by dc_map\n");
  fprintf(stderr,"\t-s<sector>      list of sectors to fit coherently.\n")
;
  fprintf(stderr,"\t-n<#events>     number of events to process. 0 for all (1000)\n");
  fprintf(stderr,"\t-j<#events>     number of events to skip/jump over (0)\n");
  fprintf(stderr,"\t-d<dim #>       dimension to fit (z,x,roty,rotx,y,rotz) (123 = z x roty)\n");
  fprintf(stderr,"\t-l<layertype>   1 for axial, 2 for stereo, 3 for both (1)\n");
  fprintf(stderr,"\t-rf<RegionPat>  which region group to move and fit together (2)\n");
  fprintf(stderr,"\t-M<sl><maxhole> max number of permitted holes in sl (1,2,2,2,2,2)\n");
  fprintf(stderr,"\t-w<reg><float>  factor that sigma has been multiplied by (1.,1.,1.)\n");
  fprintf(stderr,"\t-D              debugging output flag.\n");
  fprintf(stderr,"\t-W              turn on term weights by cos(theta) distribution.\n");
  fprintf(stderr,"\t-P<float>       Maximum %%difference between best and worst chi2.(%f)\n",PRECISION);
  fprintf(stderr,"\t-X<float>       maximum track chi2 (from TBER) to be used in the fit\n");
  exit(1);
}

int main(int argc, char *argv[]) {
  char *fname[NFILES];  /* filename of cooked file to use */
  int nfile = 0;
  char *inoff = NULL;  /* input dc_map style map file */
  char *outoff = NULL; /* output dc_map style map file */
  FILE *fp = NULL;
  float Precision = PRECISION;
  
  int i,j,cnt,sec,doall;
  char mess[100];

  /* EXTERNAL variables that are changed */
  extern int Sector[];
  extern int RegMove[];
  extern int MaxHoles[];
  extern int ndim;
  extern int layertype;
  extern int nevent;
  extern int jumpevnt;
  extern char *command;
  extern track_t *toptrack;
  extern float weightmult[];
  extern int debug;

  track_t *tr1,*tr2;

  int runno;
  int tmp,t;
  int dimlist;
  char *cdx[6] = { "  dz   ","  dx   "," droty "," drotx ","  dy  "," drotz " };

  dimlabel = cdx;

  if (argc == 1) PrintUsage(argv[0]);

  command = argv[0];
  for (i=0; i<NFILES; fname[i++] = NULL);

  for (i=1; i<argc; ++i) {
    if (argv[i][0] == '-') {
      switch(argv[i][1]) {
      case 'D':      /* debug output */
	debug = -1;
	break;

      case 'W':      /* use weight factor for angular distribution? */
	WeightOn = -1;
	break;

      case 'P':      /* ending condition of fit: 2.(chiworst-chibest)/(sum) <Precision */
	Precision = atof(&argv[i][2]);
	break;

      case 'X':      /* maximum chi2 (from TBER) for a track to be used in the fit */
	maxChi2 = atof(&argv[i][2]);
	break;

      case 'd':      /* list of dimensions to use */
	dimlist = tmp = atoi(&argv[i][2]);
	ndim = 0;
	while (tmp != 0 && ndim < 6) {
	  t = (tmp % 10);
	  if (t >= 1 && t <= 6) {
	    dim[ndim] = t;
	    ndim++;
	  } else {
	    fprintf(stderr,"Illegal dimension number %d, numbers go from 1 to 6.\n",t);
	    exit(1);
	  }
	  tmp /= 10;
	}
	for (t=ndim;t<6; t++)
	  dim[t] = 0;
	break;

      case 'l':      /* layer type */
	layertype = atoi(&argv[i][2]);
	break;

      case 'i':      /* input map file name */
	inoff = &argv[i][2];
	break;

      case 'o':      /* output map file name */
	outoff = &argv[i][2];
	break;

      case 'n':      /* number of events to accept */
	nevent = atoi(&argv[i][2]);
	break;

      case 'j':      /* number of events to ignore (Jump over) */
	jumpevnt = atoi(&argv[i][2]);
	break;

      case 's':      /* select the sector number: 0 = apply offsets to all sectors R1*/
	for (j=1; j <= 6; Sector[j++] = 0);
	for (j=0; j < strlen(&argv[i][2]); j++) {
	  int s = argv[i][2+j]-'0';
	  if (s>=0 && s <=6)
	    Sector[s] = -1;
	  else {
	    fprintf(stderr,"Error option -s: Bad sector number %c\n",argv[i][2+j]);
	    PrintUsage(argv[0]);
	  }
	}
	break;

      case 'w':      /* value to divide sigma from xvst function by */
	{
	  int reg = argv[i][2] - '0';
	  if ( (reg >= 1) && (reg <= 3) ) { /* good region number */
	    weightmult[reg-1] = atof(&argv[i][3]);
	  } else {
	    fprintf(stderr,"Error bad region number in %s\n",argv[i]);
	    PrintUsage(argv[0]);
	  }
	}
	break;

      case 'r':
	switch(argv[i][2]){
	  int reg,j;
	case 'f': /* set of regions to calculate chi2 from and move */
	  for (j=0; j < 3; RegMove[j++] = 0);
	  for (j=0; j < strlen(&argv[i][3]); j++) {
	    if ( (reg = argv[i][3+j]-'1') > -1 && reg < 3) {
	      RegMove[reg] = -1;
	    } else {
	      fprintf(stderr,"Error option -rf: Bad region number %c\n",argv[i][3+j]);
	      PrintUsage(argv[0]);
	    }
	  }
	  break;
	default:
	  fprintf(stderr,"Error option %s is undefined.\n",argv[i]);
	  PrintUsage(argv[0]);
	  break;
	} /* r switch */
	break;
	
      case 'M':
	{
	  int sl = argv[i][2] - '0';
	  if ( (sl >=1) && (sl <= 6) )  { /* good superlayer number */
	    MaxHoles[sl-1] = atoi(&argv[i][3]);
	  }
	  else {
	    fprintf(stderr,"Error option -M: Bad SL number %c\n",argv[i][2]);
	  }
	}
	break;

      default:
	fprintf(stderr,"Error option %s is undefined.\n",argv[i]);
	PrintUsage(argv[0]);
	break;
      }
    } else {
      if (nfile < NFILES) {
	fname[nfile] = argv[i];
	nfile++;
      } else {
	fprintf(stderr,"Error: Cannot specify more than %d files.\n",NFILES);
	exit(15);
      }
    }
  } /* done with command line switches */

  if ( !(fname[0]) || !(outoff) ) { /* didn't specify output filename or cooked file */
    fprintf(stderr,"Error: Need to specify -o<results file> and file to process.\n");
    PrintUsage(argv[0]);
  }

  /* count the number of dimensions specified */
  for (ndim = 0; (ndim<6) && dim[ndim]; ndim++);

  if (ndim < 1 || ndim > 7) {
    fprintf(stderr,"Error: incorrect number of dimensions.\n");
    PrintUsage(argv[0]);
  }

  /* see if we are aligning multiple sectors coherently */
  tmp = 0;
  for (j=0; j<=6; j++) {  /* from 0 so that -s05 aligns all of R1 wrt sector 5 */
    Sector[0] |= (tmp && Sector[j]);   /* if had a sector, and see another, coherent */
    tmp |= Sector[j];
  }
  
  if (!tmp) {
    fprintf(stderr,"Error: Need to specify sector(s).\n");
    PrintUsage(argv[0]);
  }

  printf("PARAMETERS for this run of %s\n",argv[0]);
  printf("\tFiles: ");
  for (i=0; i<nfile; i++)
    printf(" %s  ",fname[i]);
  
  printf("\n\tjumpEvents: %i, numEvents: %i\n",jumpevnt,nevent);
  printf("\tLayers: %i, dim: %i, Cos(theta) Wgt: %i\n",layertype,dimlist,WeightOn);
  printf("\tSigma multiplier: %f, %f, %f,  Precision: %f\n",
	 weightmult[0],weightmult[1], weightmult[2],Precision);
  printf("\tInput offsets: %s, Output offsets: %s\n",
	 (inoff == NULL) ? "<map>" : inoff,outoff);
  printf("\tSector: %i,%i,%i,%i,%i,%i,%i,   RegMove: %i,%i,%i\n",
	 Sector[0],Sector[1],Sector[2],Sector[3],Sector[4],Sector[5],Sector[6],
	 RegMove[0],RegMove[1],RegMove[2]);
  printf("\tMaxHoles: %d, %d, %d, %d, %d, %d\n",
	 MaxHoles[0],MaxHoles[1],MaxHoles[2],MaxHoles[3],
	 MaxHoles[4],MaxHoles[5]);
  
  init_bos_banks();

  if(!(toptrack = getTBLAtoTRACKS(fname,nfile,&runno))) {
    fprintf(stderr,"Error: no tracks read in.\n");
    exit(1);
  }

  /* get map parameters either from the map, or from file */
  if (inoff) { /* from file */
    if (fp = fopen(inoff,"r")) {
      read_dc_text(fp,&runno,xt,st,xpos[0],xpos[1],xpos[2],sdir[0],sdir[1],sdir[2]);
      fclose(fp);
    } else {
      fprintf(stderr,"%s: Cannot open file \"%s\"\n",command,inoff);
      exit(1);
    }
  } else { /* from the Map */
    float xtmp[3][6][3],stmp[3][6][3];
    int ok[3][6],reg,sec;
    
    if (get_dc_map_geom_(&runno,xtmp,stmp,xt,st,ok)) {
      for (i=0;i<3;i++)
	for(reg=0;reg<3;reg++)
	  for(sec=0;sec<6;sec++) {
	    xpos[i][DC_INDEX(sec,reg)] = xtmp[reg][sec][i];
	    sdir[i][DC_INDEX(sec,reg)] = stmp[reg][sec][i];
	  }
    } else {
      fprintf(stderr,"%s: Cannot read DC map\n",command);
      exit(1);
    }
  }

  /* now have offsets. now, create dgeo, dcgm, dcgw banks, and ready to go */

  printf("Initialization complete.\n");

  doAmoeba(Precision); /* at the end, xpos, sdir are modified in here */

  if ( (fp = fopen(outoff,"w")) ) {
    write_dc_text(fp,-2,xt,st,xpos[0],xpos[1],xpos[2],sdir[0],sdir[1],sdir[2]);
    fflush(fp);
    fclose(fp);
  } else {
    fprintf(stderr,"Cannot write to %s\n",outoff);
    write_dc_text(stdout,-2,xt,st,xpos[0],xpos[1],xpos[2],sdir[0],sdir[1],sdir[2]);
  }

  /* free the track list */
  for (tr1 = toptrack; tr1 != NULL; tr1 = tr2) {
    tr2 = tr1->next;
    free(tr1);
  }
  return 0;
  
}

track_t *getTBLAtoTRACKS(char *fname[], int nfiles, int *runno) {
  /*  reads in the input file, converts the TBLA bank to the track list,
   *  fills the runno, and returns a pointer to the top of the track
   *  linked list
   */

  extern track_t *toptrack;

  track_t *track = NULL;
  int cnt = 0;
  int skipcnt = 0;
  int i,l;
  char mess[100];
  clasTBTR_t *TBTR;
  clasTBER_t *TBER;
  clasTBLA_t *TBLA;
  int evnt;
  int ntrck = 0;
  int err;

  *runno = 0;
  
  
  for (i=0;i < nfiles; i++) {
    sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ",fname[i]);
    if (!fparm_c(mess)) {
      fprintf(stderr,"%s: Unable to open file \"%s\"\n",command,fname[i]);
      exit(1);
    } else {

    /* if we've been asked to skip over some events, do so */
      while ( (skipcnt < jumpevnt) && (getBOS(&bcs_,1,"E"))) {
	dropAllBanks(&bcs_,"E");
	cleanBanks(&bcs_);
	skipcnt++;
	if (skipcnt % 100 == 0) {
	  fprintf(stderr,"Skipping %d\r",skipcnt);
	  fflush(stderr);
	}
      }

      /* read in the TBLA banks, and get the runno */
      while ((nevent ? cnt < nevent : 1) && (getBOS(&bcs_,1,"E"))) {
	clasHEAD_t *hdr;
	
	hdr = getBank(&bcs_,"HEAD");
	if (*runno != hdr->head[0].nrun) { /* new run number */
	  *runno = hdr->head[0].nrun;
	  printf("Run number: %i\n",*runno);
	}
	evnt = hdr->head[0].nevent;
	cnt++;

	if (cnt % 100 == 0) { /* display number of read in events so far */
	  fprintf(stderr,"Processing %d\r",cnt);
	  fflush(stderr);
	}
	
	if (err=filltrack(&track,evnt)) {
	  fprintf(stderr,"ERROR # %d reading in tracks. evnt = \n",err,evnt);
	  exit(err);
	}
	dropAllBanks(&bcs_,"E");
	cleanBanks(&bcs_);
      }
    }

    sprintf(mess,"CLOSE BOSINPUT");
    if (!fparm_c(mess)) {
      fprintf(stderr,"%s: Unable to close file \"%s\"\n",command,fname[i]);
      break;
    }
  }
  /* walk through and count tracks in memory */
  for (track = toptrack,i=0; track != NULL; track = track->next, i++) {
    track->weight = weight(track);
  }
  
  fprintf(stdout,"Total of %d tracks read in, %d used in the fit.\n",cnt,i);
  return toptrack;
}
		    
float chi2(track_t *track,float *nhits) {
  /* calculates the chi2 contribution for track *track.
   * returns the chi2 term and the number of hits used on the track.
   */

  int i;
  int sl,sec,reg,lay,wir;
  float chi2_term=0.;
  float residual,trsigma;
  float thisterm;
  float sigma_sqr;
  hit_t thishit;
  int laygood;
  extern int debug;
  float trackChi2;

  static int debugcnt = 0;

  *nhits = 0.;

  for (i = 0; i < 34; i++) {
    thishit = track->hits[i];
    sl = ( (lay = thishit.layer) - 1)/6 + 1;
    reg = ( sl - 1)/2;

    if (layertype == 1)              /* what type of layers are we to include */
      laygood = (sl==2)||(sl==3)||(sl==5);
    else if (layertype == 2)           /* stereo */
      laygood = (sl==1)||(sl==4)||(sl==6);
    else if (layertype == 3)           /* all */
      laygood = 1;
    else {
      laygood = 0;
      fprintf(stderr,"ERROR: layertype incorrect = %i\n",layertype);
      exit(1);
    }

    sec = track->sector;
    wir = thishit.wire;
    //if(track->chi2 > maxChi2) printf("chi2: %f\n", track->chi2);
    // Added chi2 cut. Matt Bellis 06/08/05
    if (RegMove[reg] && laygood && (wir > 0) && track->chi2 < maxChi2) {
      (*nhits) += track->weight;
 /*
  *  modified next line to do difference of fabs
  *    residual = (doca(track,&(track->hits[i]),&trsigma)-thishit.tdoca);
  */
      residual = (fabs(doca(track,&(track->hits[i]),&trsigma))-fabs(thishit.tdoca));
      sigma_sqr = trsigma+SQR(thishit.sigma);
      thisterm = SQR( residual )/sigma_sqr;
      if (debug) {
	if (debugcnt % 30 == 0) {
	  printf(" evnt  trk Lay Wir TRACKsg^2  TIMEsg^2 SGma^2 Residual Wgt   TERM\n");
	  debugcnt = 0;
	}
	debugcnt++;
	printf("%7d:%2d,(%2d,%3d) %9.3f %8.3f %9.3f %7.3f %6.3f %6.3f\n",
	       track->event, track->number, lay, wir,
	       trsigma, SQR(thishit.sigma), sigma_sqr, residual, track->weight,
	       thisterm);
      }
      chi2_term += track->weight*thisterm;
    }
  }
  return chi2_term;
}

float doca(track_t *track, hit_t *hit, float *trsigma) {
  /* calculate distance of closest approach of track described by point pos
   * and direction dir to wire (sector,layer,wire).
   * Input:  pos
   *         dir
   *         sec
   *       layer
   *        wire
   */

  vector3_t wpos,wdir,tmp,pos,dir;
  float mtmp;

  clasDCGW_t *DCGW = getGroup(&wcs_,"DCGW",track->sector);
  int wir_ind = (hit->layer-1)*192+hit->wire-1;
  dcgw_t dcgw;

  pos = track->pos;
  dir = track->dir;

  if (DCGW) {
    dcgw = DCGW->dcgw[wir_ind];
    wpos.x = dcgw.x_mid;
    wpos.y = dcgw.y_mid;
    wpos.z = dcgw.z_mid;
    wdir.x = dcgw.x_dir;
    wdir.y = dcgw.y_dir;
    wdir.z = dcgw.z_dir;

    *trsigma = get_trsigma(track,hit,wpos,wdir);

    /*    *trsigma = tracksigma(track,wpos,wdir); */

    tmp = v3cross(wdir,dir);
    mtmp = v3mag(tmp);
    if (mtmp != 0.)
      return v3dot(tmp,v3sub(pos,wpos))/mtmp;
    else {
      fprintf(stderr,"ERROR: wire and track in same direction\n");
      return 0.;
    }
  } else {
    fprintf(stderr,"ERROR: Cannot get DCGW bank\n");
    return 0.;
  }
}


double evaluate(int dummy, int ndim, float *point) {
  /* calculates the normalized chi2 for a given
   * point (set of offsets from the initial offsets).
   */

  /* dummy present just for consistency with amoeba */

  double dxR1[3] = {0., 0., 0.};
  double dsR1[3] = {0., 0., 0.};
  double dxtot[6][3] = { {0.,0.,0.},{0.,0.,0.},{0.,0.,0.},
                         {0.,0.,0.},{0.,0.,0.},{0.,0.,0.} };
  double dstot[6][3] = { {0.,0.,0.},{0.,0.,0.},{0.,0.,0.},
                         {0.,0.,0.},{0.,0.,0.},{0.,0.,0.} };


  float tmpx[3][18], tmps[3][18];
  track_t *track;

  double ret;
  float chi2_dof;
  int i,t;
  double tot_chi2;   /* double since summing many small numbers to a large one */
  float nhits;
  double tot_nhits;  /* final total large, but individual sums are small */
  static cnt = 0;
  static int sector = -1;

  if (sector < 0) {  /* first time through */
    for (i=1; i<=6; i++) {
      if (Sector[i])
	sector = i;
    }
  }

  cnt++;
  if (Sector[0]) {   /* move chamber sectors coherently */
    for (i=1; i<=ndim; i++) {
      t = dim[i-1];                 /* lookup which dimesion is meant */
      switch (t) {
      case 1: dxR1[2] = point[i];  /* z */
        break;
      case 2: dxR1[0] = point[i];  /* x */
        break;
      case 3: dsR1[1] = point[i]; /* roty */
        break;
      case 4: dsR1[0] = point[i];  /* rotx */
        break;
      case 5: dxR1[1] = point[i];  /* y */
        break;
      case 6: dsR1[2] = point[i];  /* rotz */
        break;
      default:
        fprintf(stderr,"Shouldn't be here\n");
        break;
      }
    }
    R1TransRot(dxR1,dsR1,dxtot,dstot);
  } else {

  
    for (i=1; i<=ndim; i++) {
      t = dim[i-1];
      switch (t) {
      case 1: dxtot[sector-1][2] = point[i];  /* z */
        break;
      case 2: dxtot[sector-1][0] = point[i];  /* x */
        break;
      case 3: dstot[sector-1][1] = point[i]; /* roty */
        break;
      case 4: dstot[sector-1][0] = point[i];  /* rotx */
        break;
      case 5: dxtot[sector-1][1] = point[i];  /* y */
        break;
      case 6: dstot[sector-1][2] = point[i];  /* rotz */
        break;
      default:
        fprintf(stderr,"Shouldn't be here\n");
        break;
      }
    }
  }

  addOff_Cons_BRot(dxtot,dstot,xpos,sdir,tmpx,tmps,RegMove);
  intoDGEO(xt,st,tmpx,tmps);
  dc_geom_banks_();

  tot_nhits = 0;
  tot_chi2 = 0.;
  track = toptrack;
  while(track) {
    tot_chi2 += chi2(track,&nhits);
    tot_nhits += nhits;
    track = track->next;
  }
  chi2_dof = tot_chi2/(tot_nhits - ndim);
  ret = tot_chi2;
  printf("  Evaluation %i: wgt_tot: %e  chi2/v: %e  pt(",cnt,tot_nhits,chi2_dof);
  for (i=1; i<= ndim; i++) {
    printf(" %7.4f",point[i]);
  }
  printf(")\n");

  return ret;
}


int doAmoeba(float Precision) {
  int i,j,t;
  extern int Sector[];
  extern int dim[];
  int sector = -1;
  float **pts;
  double *y;
  float *ytry, *start;
  float ftol;

  double dxR1[3] = { 0., 0., 0.};
  double dsR1[3] = { 0., 0., 0.};
  double dxtot[6][3] = { {0.,0.,0.},{0.,0.,0.},{0.,0.,0.},
                         {0.,0.,0.},{0.,0.,0.},{0.,0.,0.} };
  double dstot[6][3] = { {0.,0.,0.},{0.,0.,0.},{0.,0.,0.},
                         {0.,0.,0.},{0.,0.,0.},{0.,0.,0.} };
  float tmpx[3][18],tmps[3][18];

  int ntrials;

  int ncalls;


  /* first of all, setup to evaluate the tracking at the points around the area
   * of interest.
   */
  pts = matrix(1,ndim+1,1,ndim);
  y = dvector(1,ndim+1);
  ytry = vector(1,ndim);
  start = vector(1,ndim);

  if (sector < 0) {
    for (i=1; i<=6; i++)
      if (Sector[i])
	sector = i;
  }

  for (i=1;i<=ndim+1;i++) {
    for (j=1;j<=ndim;pts[i][j++] = 0.);
    y[i] = 0.;
  }
  /* range over which to look for a better starting point */

  for (i=1; i<=ndim; i++) {
    t = dim[i-1];
    switch (t) {
    case 1:
      start[i] = 2.*XINC;
      break;
    case 2:
      start[i] = 2.*XINC;
      break;
    case 3:
      start[i] = 2.*SINCY;
      break;
    case 4:
      start[i] = 2.*SINCX;
      break;
    case 5:
      start[i] = 2.*XINC;
      break;
    case 6:
      start[i] = 2.*SINCZ;
      break;
    default:
      fprintf(stderr,"doAmoeba: Should not have t = %d\n",t);
      exit(1);
    }
  }
      
  ntrials = 1;
  printf("Beginning to do initial random search with %i trials\n",ntrials);
  y[1] = StartPoint(sector,ndim,ntrials,start);

  /* copy this interesting starting point into all the pnts */

  for (i=1; i<=ndim; i++)
    for (j=1; j<=ndim+1; j++)
      pts[j][i] = start[i];


  for (i=1; i<=ndim; i++) {
    t = dim[i-1];
    switch (t) {
      case 1: pts[i+1][i] +=  XINC;  /* z */
        break;
      case 2: pts[i+1][i] +=  XINC;  /* x */
        break;
      case 3: pts[i+1][i] +=  SINCY; /* roty */
        break;
      case 4: pts[i+1][i] +=  SINCX;  /* rotx */
        break;
      case 5: pts[i+1][i] +=  XINC;  /* y */
        break;
      case 6: pts[i+1][i] +=  SINCZ;  /* rotz */
        break;
      default:
        fprintf(stderr,"Shouldn't be here\n");
        break;
    }

    for (j=1;j<=ndim;j++)
      ytry[j] = pts[i+1][j];


      y[i+1] = evaluate(sector,ndim,ytry);
  }
  printf("Starting points are: ");
  for (i=0; i<ndim; i++)
    printf(" %s ",dimlabel[dim[i]-1]);
  printf(" chisq \n");
  
  for (i=1; i<=ndim+1; i++) {
    printf("\t Pt %i:  ",i);
    for (j=1; j<=ndim; j++)
      printf("%8.5f  ",pts[i][j]);
    printf("%f\n",y[i]);
  }

  /* pick the ftol according to whatever is smaller, Precision or 1/chi2[0] so
   * we can be sure that the final points are at least within chi2 +/- 1.
   */

  ftol = FMIN(Precision, 10./y[1]);

  printf("PRECISION required for end is: %e\n",ftol);

  /* the setjmp is just inside amoeba, so set up the signal catching
   * very close to how they will be handled.
   */
  signal(SIGINT, signal_handler);
  signal(SIGHUP, signal_handler);

  /* now do the amoeba fit... */
  printf("Before amoeba......%d",sector);
  amoeba(sector,pts,y,ndim,ftol,evaluate,&ncalls);


  /* finished with the fit, add our best offsets to the original set */

  if (Sector[0]) {       /* can only do R1 with all sectors */

    for (i=1; i<=ndim; i++) {
      t = dim[i-1];
      switch (t) {
      case 1: dxR1[2] = pts[1][i];  /* z */
        break;
      case 2: dxR1[0] = pts[1][i];  /* x */
        break;
      case 3: dsR1[1] = pts[1][i]; /* roty */
        break;
      case 4: dsR1[0] = pts[1][i];  /* rotx */
        break;
      case 5: dxR1[1] = pts[1][i];  /* y */
        break;
      case 6: dsR1[2] = pts[1][i];  /* rotz */
        break;
      default:
        fprintf(stderr,"Shouldn't be here\n");
        break;
      }
    }
    R1TransRot(dxR1,dsR1,dxtot,dstot);
  } else {
    for (i=1; i<=ndim; i++) {
      t = dim[i-1];
      switch (t) {
      case 1: dxtot[sector-1][2] = pts[1][i];  /* z */
        break;
      case 2: dxtot[sector-1][0] = pts[1][i];  /* x */
        break;
      case 3: dstot[sector-1][1] = pts[1][i]; /* roty */
        break;
      case 4: dstot[sector-1][0] = pts[1][i];  /* rotx */
        break;
      case 5: dxtot[sector-1][1] = pts[1][i];  /* y */
        break;
      case 6: dstot[sector-1][2] = pts[1][i];  /* rotz */
        break;
      default:
        fprintf(stderr,"Shouldn't be here\n");
        break;
      }
    }
  }
  addOff_Cons_BRot(dxtot,dstot,xpos,sdir,xpos,sdir,RegMove);

  /* offsets up to date. evaluate the errors */
  SigmaParam(ytry,ndim);

  /* display our results */
  printf("\n\n");
  printf("Pts: ");
  for (i=0; i<ndim; i++)
    printf(" %s ",dimlabel[dim[i]-1]);
  printf("  chisq\n");
;
  for (i=1;i<=ndim+1;i++) {
    printf(" %i:   ",i);
    for (j=1; j<=ndim; j++)
      printf("%8.5f  ",pts[i][j]);
    printf("%f\n",y[i]);
  }

  printf("err:  ");
  for (i=1; i<=ndim;i++)
    printf("%8.5f  ",ytry[i]);

  printf("\n");

  free_matrix(pts,1,ndim+1,1,ndim);
  free_dvector(y,1,ndim+1);
  free_vector(start,1,ndim);
  free_vector(ytry,1,ndim);

  return ncalls;
}

static void signal_handler(int sig) {
  switch (sig) {
  case SIGINT: /* interrupt */
    fprintf(stderr,"\n\t** INTERRUPT SIGNAL **\n");
    fprintf(stderr,"\t Cleaning up. We will finished in a few seconds.\n");
    longjmp(env,sig);
    /* doesn't make it to break */
  case SIGHUP: /* hangup */
    fprintf(stderr,"\n\t** HANGUP SIGNAL **\n");
    fprintf(stderr,"\t Cleaning up. We will finished in a few seconds.\n");
    longjmp(env,sig);
    /* doesn't make it to break */
  default:
    exit(sig);
  }
}

double StartPoint(int sector, int ndim, int trials, float *dxs) {
  /* does a number of initial runs, randomly sampling the space spanned by vector
   * dxs. Returns in these values the best set of initial values
   * to start at.  trials is the number of random throws to do.
   * dxs is pointer to vector telling the range (+/-) to throw over.
   * 
   * Looks around the normal position (starting xpos,sdir).
   */

  float *pnt;
  float *best;
  double chibest = 1000000000000.;
  double thischi;
  int ntry,i,j,t;


  pnt = vector(1,ndim);
  best = vector(1,ndim);

  /* first evaluate at our current spot */

  for (i=1; i<=ndim; best[i++] = 0.);
  chibest = evaluate(sector,ndim,best);

  
  for (ntry=2; ntry <= trials; ntry++) {
    
    for (i=1; i<=ndim; i++)
      pnt[i] = 2.*dxs[i]*rand()/RAND_MAX - dxs[i];
    
    thischi = evaluate(sector,ndim,pnt);
    if (thischi < chibest) {
      chibest = thischi;
      for (i=1; i<=ndim; i++)
        best[i] = pnt[i];
      printf("\tBETTER POINT FOUND\n");
    }
  }

  for (i=1; i<=ndim; i++)
    dxs[i] = best[i];

  free_vector(best,1,ndim);
  free_vector(pnt,1,ndim);
  
  return chibest;
}

void SigmaParam(float *err, int ndim) {
  /* evaluate the error on the parameters, by varying them one at time
   * (not entirely proper, but a slight overestimate of the error).
   * Assumes a parabolic shape to the function about the minimum.
   *
   *  pt  : vector (from 1..ndim) returned to contain the sigma estimates
   *  ndim: int    number of dimensions that were fit
   */

  double chisq1, chisq2, chisq3;  /* we're going to be subtracting large numbers
				   * for a small difference. */
  extern int Sector[];
  static int sector = -1;
  int i,t;
  float *pt;

  if (sector < 0) {
    for (i=1; i<=6; i++)
      if (Sector[i])
	sector = i;
  }
  
  pt = vector(1,ndim);
  for (i=1; i<=ndim; i++)
    pt[i] = 0.;
  
  /* evaluate at base point */
  chisq2 = evaluate(sector,ndim,pt);

  /* for each coordinate being modified, apply an offset */
  for (i = 1; i <=ndim; i++) {
    t = dim[i-1];
    switch(t) {
    case 1:
      pt[i] = -XINC/20.;   /* z */
      break;
    case 2:
      pt[i] = -XINC/20.;   /* x */
      break;
    case 3:
      pt[i] = -SINCY/20.;  /* roty */
      break;
    case 4:
      pt[i] = -SINCX/20.;  /* rotx */
      break;
    case 5:
      pt[i] = -XINC/20.;   /* y */
      break;
    case 6:
      pt[i] = -SINCZ/20.;  /* rotz */
      break;
    default:
      fprintf(stderr,"SigmaParam: Shouldn't be here\n");
      break;
    }

    /* evaluation point all set. evaluate away */
    chisq1 = evaluate(sector,ndim,pt);  /* at -delta */
    pt[i] = - pt[i];
    chisq3 = evaluate(sector,ndim,pt);  /* at +delta */
    
    err[i] = pt[i]*sqrt(2./(chisq1-2.*chisq2+chisq3));
    pt[i] = 0.;     /* zero this parameter to be ready for the next */
  }
  return;
}

void init_bos_banks (void) {
  /* set up named bank formats and the bos area */
  /* common blocks for fpack and cern only once (just from a1) */
  int max = 1000;
  /*  int memh = MEMH; */

  bnames_(&max);
  initbos();
  configure_banks(stderr,-1);
  /*  hlimit_(&memh); */
  bankList(&bcs_, "C=","TDPLHDPLHEADCALLHBTRTRKSDCLATBLADTRKTBTRHBERTBERHBLATBLAHBTB"
	   "SC1 SCR SCRCHBIDDC1 CC01CCRCEC01ECPIECHBEVNTEVHBDCPBECPBSCPBCCPBUNUS"
	   "HEVTCL01TBIDPARTEC1RTAGRTAGIST1 STR RF  ");  /*Cooked list*/
  bankList(&bcs_, "R=","RC01RC02RC03RC04RC05RC06RC07RC08RC09RC10RC11RC12RC13RC14"
	   "RC15RC16"); /*raw list*/
}

float Dth(float th) {
  /* distribution function parameterized by theta */
  static float par[7] =
    /* { 220.1, 0.3604, -0.1041, -49400., 344700., -5.656, -2.878 } */
  { 175.1, 0.2928, -0.2160, -15.31, 55.93, 3.089, -2.774 };
    
  
  static float *p = par-1;
  float g,p1,e;

  g = p[1]*exp(.5*SQR((th-p[2])/p[3]));
  p1 = p[4]+p[5]*th;
  e = exp(p[6]+p[7]*th);

  return g+p1*e;
}

float Dcosth(float costh) {
  /* distribution function parameterized by cos(theta) */
  static float par[4] =
  { -1.2421, 10.054, 134.13, 162.21 };

  static float *p = par-1;
  float e, p1;

  e = exp(p[1] + p[2]*costh);
  p1 = p[3] + p[4]*costh;

  return e+p1;
}

float weight(track_t *track) {
  /* weights the track by: the theta distribution
   */
  extern int WeightOn;
  static float Dcosth0 = -1.;
  /* store up counts so that there is the same weight per 10 degree bin,
   * from 20 to 100 degrees */
#define NTHETA 8
  static float dtheta = 10., mintheta = 20., maxtheta = 100.;
  float theta;
  int thetabin;
  static float angcnt[6][NTHETA];
  static float rtod = -1;
  static int first_after = -1;
  int i, j, sum;

  if (Dcosth0 < 0.) { /* first time through */
    Dcosth0 = Dcosth(.6);
    rtod = 180./acos(-1.);
    memset(angcnt,0,sizeof angcnt);
  }

  theta = acos(track->dir.z)*rtod;
  thetabin = (int)((theta-mintheta)/dtheta);
  if (track->weight == 0.) {
    if (thetabin >= 0 && thetabin < NTHETA) {
      angcnt[track->sector-1][thetabin]++;
    }
  } else {
    if (first_after) {
      first_after = 0;
      for (i=0; i<6; i++) {
	for (j=0, sum=0; j<NTHETA; j++)
	  sum += angcnt[i][j];
	for (j=0; j<NTHETA; j++)
	  if (angcnt[i][j] != 0)
	    angcnt[i][j] = sum/angcnt[i][j];
      }
    }
    if (WeightOn)
      /*      return Dcosth0/Dcosth(track->dir.z); */
      if (thetabin >=0 && thetabin < NTHETA)
	return angcnt[track->sector-1][thetabin];
      else
	return 0;
    else
      return 1.;
  }
  return 1.;
}

int cuts(track_t *track) {
  /* cut the tracks read in by:
   * a) angular weighting. theta distribution follows p1*e 
   */

#ifndef RAND_MAX
#define RAND_MAX (pow(2.,15)-1)
#endif

  static float Dth0 = -1.;
  float th;

  if (Dth0 < 0.) {  /* first time through */
    Dth0 = Dth(1.15);
  }
  th = acos(track->dir.z);
  if (rand()/(float)RAND_MAX < Dth0/Dth(th))
    return -1;
  else
    return 0;
}

