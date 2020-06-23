/*
 *  Program: sc_delay_el ( for electron beam data )
 *           calculate relative delays for each of 
 *           the counters.
 *  Author : Konstantin Loukachine
 *
 *  Date :   May, 1st, 1999. 
 *
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <map_manager.h>
#include <sc.h>
#include <particleType.h>
#include <kinematics.h>
#include <utility.h>
#include <pid.h>
#include <call.h>
#include <vertex.h>

/* PAW defines */

#define MEMH 500000
#define LREC 1024 /* record length of hbook direct access file in WORDS */
#define LUN 3 /* logical unit number of hbook file */
#define ID 99 /* column-wise-ntuple id */
#define RAD2DEG (180.0/3.14159)

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* declare the hbook common */
float pawc_[MEMH];
float hcfitd_[25];
int quest_[100];

/* ----------- Function prototypes ------------------ */

void PrintUsage(char *processName);
int ProcessEvent(float beam_energy);
void ctrlCHandle(int);
void book_histos(int current_run);
void hini(char *out, int current_run);
int fit(int current_run);
/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stderr,"\n");
  fprintf(stderr,"  Usage: %s [-n#] -b inputfile\n\n",processName);
  fprintf(stderr,"  Options:\n\n");
  fprintf(stderr,"\t-n[#]\t\tProcess only # number of events \n\n");
  fprintf(stderr,"\t-b\t\tSilent mode \n\n");
  fprintf(stderr,"\t-h\t\tPrint this message.\n\n");
  fprintf(stderr,"\t HBOOK output by default: run[#]_dt.hbk\n");
  fprintf(stderr,"\t Text output by default: run[#]_dt.txt\n\n");
  exit(0);
}

main(int argc,char **argv)
{  
  FILE *fp = NULL;
  int i,j,icycle,firsttime;
  char *argptr;
  int Nevents = 0, max = 0;
  int current_run = 0;
  int silence = 0;
  float beam_energy = 0, c2c_map[288];
  char mess[1000], outfile[100];  
  char  *dir, def_map[256];

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'b':
	silence = 1;
	break;
      case 'n':
	max = atoi(++argptr);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }


  fprintf(stderr,"* PID and FC result banks will be rebuilt\n");

  initbos();

  for (i = 1;i < argc; ++i) {
  argptr = argv[i];
    if (*argptr != '-') {
    sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
    if (!fparm_c(mess)) {
    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
    }
      else {
	while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")){
        clasHEAD_t *HEAD = getBank(&bcs_, "HEAD"); 
	int j;
        
        if(HEAD){
	  Nevents++;
          current_run = HEAD->head[0].nrun;
          if(Nevents == 1){
          sprintf(outfile, "run%d_dt.hbook", current_run); 
          hini(outfile,current_run); 
    dir = getenv("CLAS_PARMS");
    sprintf(def_map,"%s/Maps/RUN_CONTROL.map",dir);
    map_get_float(def_map, "beam","energy", 1, &beam_energy,current_run,&firsttime);
    beam_energy = beam_energy/1000.0; 
    fprintf(stderr,"* Beam energy: %6.3f\n", beam_energy);  
         /* Initialize the CC package */
            cc_init(current_run);    
         /* Initialize CL01 bank  */
            initCL01(current_run);    
         /* Initialize the SC package */
            initialize_tof(current_run);
         /* Initialize the EC package */ 
            ec_set_def_();
            ec_brun_(&current_run);
         /* make RUNC bank */
            make_RUNC_bank(current_run);
	  }

	  if (Nevents % 100 == 0 && silence != 1) {
	  fprintf(stderr,"%d\r",Nevents);
	  fflush(stderr);
	  }

	ProcessEvent(beam_energy);        

	dropAllBanks(&bcs_,"E");
	cleanBanks(&bcs_);

	}
	}  
     
	fprintf(stderr,"#  of events processed: %d\n",Nevents);
	/*close file*/
	/*sprintf(mess,"CLOSE BOSINPUT", argptr);
	fprintf(stderr, "err = %d", fparm_c(mess));*/
      }    
    }
  }

  fit(current_run); 

  hrout(0,icycle," ");
  hldir_(" ", " ", 1L, 1L); /* don't remove this line */
  hrend_("esr", 3L);

}

void hini(char *out, int current_run){
  int lun = LUN, lrec = LREC, memh = MEMH;
  int istat, icycle;   
  char def_out[100];

  quest_[9] = 65000;
  hlimit_(&memh);
  hropen_(&lun, "esr", out , "N", &lrec, &istat, 3L, strlen(out), 1L);
  book_histos(current_run);
}

void book_histos(int current_run){
    int i,j;
    char title[100];

    for(j=1; j <= 6; j++) {
    sprintf(title, "[B] vs P, sector %d", j);
    hbook2(10*j+1,title,100,0.0,4.0,100,0.0,1.2,0);
    sprintf(title, "P vs M, sector %d", j);
    hbook2(10*j+2,title,100,0.0,1.5,100,0.0,4.0,0);
    sprintf(title, "Mass vs Counter No, sector %d", j);
    hbook2(10*j+3,title,48,0.0,48.0,100,0.0,1.5,0);
    sprintf(title, "[d]E vs P, sector %d", j);
    hbook2(10*j+4,title,100,0.0,3.0,100,0.0,80.,0);
           for(i=1; i <= 48; i++) {
           sprintf(title, "[D]t, sec %d, cntr %d",j,i);
           hbook1(100*j+i,title,30,-1.5,1.5,0);  
	   } 
    }
}

int ProcessEvent(float beam_energy){

  clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
  clasBID_t *TBID = NULL;
  clasPART_t *PART = NULL;
  clasCL01_t *CL01 = NULL; 
  clasVERT_t *VERT = NULL;  
  clasSCRC_t *SCRC = NULL; 
  clasSCR_t *SCR = NULL;  
  clasRUNC_t *RUNC = NULL; 
  clasTBTR_t *TBTR = NULL; 
  clasTDPL_t *TDPL = NULL; 
  tdpl_t *tdpl;
  vector4_t  Particle,Beam,Target,Electron,Photon,Resonance;
  vector3_t  Vertex_part;
  int   i,j,part_id,stat_ec,stat_sc,index_tbid,sec,sc_nn,cntr_nn;
  int   sector, tof_id, index_tbtr;
  float vtx_time,rf_correct,rf_correct_trg,good_rf,good_rf_trg; 
  float Nu, Q2, W, el_vtime, part_vtime, track_length;
  float Part_mass_sq, Part_mom, Part_mass, charge, Part_beta, dedx;
  float Time_pion, Time_pion_tof;

/*  rebuilding the banks */

        RUNC = getBank(&wcs_,"RUNC");
        dropAllBanks(&bcs_,"SC1 SCR SCRCCC01ECHBCL01");
        for(sec=1; sec <= 6; sec++) {        
          make_SC1_bank(sec);   
          make_SCR_bank(sec,"TDPL");
          make_SCRC_bank(sec);
          make_CC01_bank(sec);
        } 
        make_CL01_bank();
        ec_evnt_();
        make_TBID_group(3);
        make_PART_group(3);       
           TBID = getGroup(&bcs_, "TBID", 3);  
           PART = getGroup(&bcs_, "PART", 3); 
           CL01 = getBank(&bcs_, "CL01");	
           TBTR = getBank(&bcs_, "TBTR");	           

    Beam.t = beam_energy;
    Beam.space.x = Beam.space.y = 0.0;
    Beam.space.z = sqrt(beam_energy*beam_energy - ELECTRON_MASS*ELECTRON_MASS);
    Target.space.x = Target.space.y = Target.space.z = 0.0;
    Target.t = PROTON_MASS;

if(HEAD && TBID && PART && TBTR){

   for (j = 0; j < PART->bank.nrow; j++){
     part_id = PART->part[j].pid;
     index_tbid = PART->part[j].trkid - 1;
     stat_ec = TBID->bid[index_tbid].ec.stat;
/* --------------------- electron stuff ----------------------------- */
   if(part_id == 3 && stat_ec == GOOD_MATCH){
    el_vtime = part_id = TBID->bid[index_tbid].sc.vtime;
    Electron = PART->part[j].p;
    Photon = v4sub(Beam, Electron);   
    Resonance = v4add(Photon, Target);
    Nu = beam_energy - Electron.t;    
    Q2 = -v4magsq(Photon);
    W  =  v4mass(Resonance);
   }
   }
/* ------------------------------------------------------------------ */

 if(Q2 > 0.0 && W > 1.1){
 
 for (j = 0; j < PART->bank.nrow; j++){
       part_id = PART->part[j].pid;
       charge = PART->part[j].q;
       index_tbid = PART->part[j].trkid - 1;
       stat_sc = TBID->bid[index_tbid].sc.stat;
 if(part_id != 3 && charge != 0 && stat_sc == GOOD_MATCH){
       index_tbtr = TBID->bid[index_tbid].track - 1;
       sector = TBID->bid[index_tbid].sec;
       TDPL = getGroup(&bcs_, "TDPL",sector);
       tdpl = tbtr2tdpl(&(TBTR->tbtr[index_tbtr]));
       track_length = pathlen2sc(&(TBID->bid[index_tbid]), (hdpl_t*)tdpl);

   Particle = PART->part[j].p;
   Vertex_part = PART->part[j].vert;
   Part_beta = TBID->bid[index_tbid].beta;

   if(Part_beta > 0.0 && Part_beta < 1.1){
   Part_mass_sq = v3magsq(Particle.space)*(1.0 - Part_beta*Part_beta)/(Part_beta*Part_beta); 
     if(Part_mass_sq > 0.0004){
     Part_mass = sqrt(Part_mass_sq);
     
     if(SCRC = getGroup(&bcs_, "SCRC",sector)){
        sc_nn = TBID->bid[index_tbid].sc.id - 1;
        cntr_nn = SCRC->scrc[sc_nn].id; 
        dedx = SCRC->scrc[sc_nn].energy;             
   
      for(j=1; j <= 6; j++){
	if(sector == j){
        hf2(10*j+1,v3mag(Particle.space),Part_beta,1.0);
        hf2(10*j+2,Part_mass,v3mag(Particle.space),1.0);
        hf2(10*j+3,(cntr_nn - 0.5),Part_mass,1.0);
        hf2(10*j+4,v3mag(Particle.space),dedx,1.0);
	}
      }

        if(dedx < 12.0 && v3mag(Particle.space) < 1.0 && Part_mass < 0.5){         
    Time_pion = track_length*sqrt(PI_CHARGED_MASS*PI_CHARGED_MASS + 
    v3magsq(Particle.space))/(29.98*v3mag(Particle.space));
        Time_pion_tof = TBID->bid[index_tbid].sc.time - TBID->bid[index_tbid].vtime;
 for(j=1; j <= 6; j++){
 for(i=1; i <= 48; i++){
 if(cntr_nn == i && sector == j) hf1(100*sector+cntr_nn,(Time_pion_tof - Time_pion),1.0);  
 }
 }
	}

     }
     }
   }
 }
 }
 }
} 
return(1);  
}

int fit(int current_run){
  int i,j,icycle,index,firsttime;
  float step[3], par[3], pmin[3], pmax[3], sigpar[3], chi2;
  float c2c[288];
  FILE *ofp;
  char number[100],*dir,sc_map[128];  

/* get numbers from the map */

  dir=getenv("CLAS_PARMS");
  sprintf(sc_map,"%s/Maps/SC_CALIBRATIONS.map",dir);
  map_get_float(sc_map, "delta_T", "paddle2paddle", 288, c2c, current_run, &firsttime);

/* fit the histograms and print out the parameters*/

  sprintf(number, "run%d_dt.txt",current_run); 
  ofp = fopen(number,"w");
  
  for (j=1; j <= 6; j++){
  for (i=1; i <= 48; i++){
      par[0] = 10.0; par[1] = 0.0; par[2] = 0.15;
      quest_[10] = 0;
      quest_[11] = 100;
      hfithn(100*j+i, "g", "rq", 0, par, step, pmin, pmax, sigpar, &chi2);
/* output to text file */
      index = 48*(j - 1)+(i - 1);
  fprintf(ofp, "%2d %5d \t %7.3f \t %6.2f \t %7.3f \t %7.3f \t <- sec,id,dt,chi2,OldMap,NewMap\n",
      j,i, par[1], chi2, c2c[index],c2c[index]+par[1]);
      par[0]=par[1]=par[2]=0.0;
  }
  }

fclose(ofp);
}

void ctrlCHandle(int x){
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t*** !!! HASTA LA VISTA !!!  ***\n\n");
  exit(1);
}

/* end file */




















