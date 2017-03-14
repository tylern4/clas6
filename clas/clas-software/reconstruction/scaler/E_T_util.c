
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ntypes.h>
#include <bostypes.h>
#include <math.h>
#include <bosddl.h>
#include <scalers.h>
#include <kinematics.h>
#include <map_manager.h>


e_t_map_t E_T_map[NUM_E_COUNTERS];
t_e_map_t T_E_map[NUM_T_COUNTERS];
float ebin_mean_map[NUM_E_BINS+1];
float ebin_min_map[NUM_E_BINS+1];
float ebin_max_map[NUM_E_BINS+1];
float tbin_min_map[NUM_T_BINS+1];
float tbin_max_map[NUM_T_BINS+1];


int T_binmax(int e_bin){
  int phys_id;
  int t_binmax = 0;

  if (e_bin >0 && e_bin <= NUM_E_BINS){
    phys_id = e_bin/2 +1;
    t_binmax = tmax(phys_id)*2;
    if (t_binmax > NUM_T_BINS) t_binmax = NUM_T_BINS;
  }
  return t_binmax;
}

int T_binmin(int e_bin){
  int t_binmin = 0;
  int phys_id;

  if (e_bin >0 && e_bin <= NUM_E_BINS){
    phys_id = e_bin/2 +1;
    t_binmin = tmin(phys_id)*2 -1;
  }
  return t_binmin;
}

int E_binmax(int t_id){
  int e_binmax = 0;

  if (t_id >0 && t_id <= NUM_T_COUNTERS){
    e_binmax = emax(t_id)*2;
    if (e_binmax > NUM_E_BINS) e_binmax = NUM_E_BINS;
  }
  return e_binmax;
}

int E_binmin(int t_id){
  int e_binmin = 0;

  if (t_id >0 && t_id <= NUM_T_COUNTERS){
    e_binmin = emin(t_id)*2 -2;
    if (e_binmin < 1) e_binmin = 1;
  }
  return e_binmin;
}

int T2Tbinmax(int t_id){
  int t_binmax = 0;

  if (t_id >0 && t_id <= NUM_T_COUNTERS){
    t_binmax = t_id*2;
    if (t_binmax > NUM_T_BINS) t_binmax = NUM_T_BINS;
  }
  return t_binmax;
}

int T2Tbinmin(int t_id){
  int t_binmin = 0;

  if (t_id >0 && t_id <= NUM_T_COUNTERS){
    t_binmin = t_id*2 -2;
    if (t_binmin < 1) t_binmin = 1;
  }
  return t_binmin;
}

int matrix2arrayind(int e_bin_target, int t_bin_target){
  int e_bin, t_bin, i=0;

  if (e_bin_target > 0 && e_bin_target <= NUM_E_BINS && t_bin_target > 0 && t_bin_target <= NUM_T_BINS){
    for (e_bin = 1; e_bin <= e_bin_target; e_bin++){  
      for(t_bin = T_binmin(e_bin); t_bin <= T_binmax(e_bin); t_bin++){
	if ((e_bin == e_bin_target) && (t_bin == t_bin_target)) return i;
	i++;
      }
    }
  }
  return -1;
}

int arrayind2matrix(int arrayind, int *e_bin_out, int *t_bin_out){
  int e_bin, t_bin, i=0;

  for (e_bin = 1; e_bin <= NUM_E_BINS; e_bin++){  
    for(t_bin = T_binmin(e_bin); t_bin <= T_binmax(e_bin); t_bin++){
      if (i == arrayind){
	*e_bin_out = e_bin;
	*t_bin_out = t_bin;
	return 1;
      }
      i++;
    }
  }
  return 0;
}

clasSCALER_t *make_TGS_from_map(BOSbank *bcs, int runno, int banknum){
  clasSCALER_t *TGS;
  tgs_t tgs = make_tgs_from_map(runno);

  if(TGS = makeBank(bcs, "TGS ",banknum, sizeof(tgs_t)/sizeof(uint32), 1)){
    memcpy(TGS->scaler, &tgs, sizeof(tgs_t));
  }
  return TGS;
}

tgs_t make_tgs_from_map(int runno){
  char *dir = getenv("CLAS_PARMS");
  char mapname[100];
  tgs_t tgs;
  int first;

  sprintf(mapname, "%s/Maps/SCALERS.map", dir);
  map_get_int(mapname, "TGS", "values", TGS_BANK_SIZE, (int *)(&tgs), runno, &first); 

  return tgs;
}

trgs_t make_trgs_from_map(int runno){
  char *dir = getenv("CLAS_PARMS");
  char mapname[100];
  trgs_t trgs;
  int first;

  sprintf(mapname, "%s/Maps/SCALERS.map", dir);
  map_get_int(mapname, "TRGS", "values", TRGS_BANK_SIZE, (int *)(&trgs), runno, &first); 

  return trgs;
}

int fill_ebin_map(){
  char *dir = getenv("CLAS_PARMS");
  FILE *fp = NULL;
  int ret = 0;
  char filename[100], fname[100];

  sprintf(filename, "%s/tagE-boundaries.dat", dir);
  if (fp = fopen(filename, "r")){
    int i;
    for (i=1; i <= NUM_E_BINS; i++){
      int dummy;
      float fdummy;
      switch(i%2){
      case 1:
	fscanf(fp, "%d %d %d %d %d %f %f %f %f\n", &dummy, &dummy, &dummy, &dummy, &dummy, &ebin_mean_map[i], &fdummy, &ebin_min_map[i], &ebin_max_map[i]);
	break;
      case 0:
	fscanf(fp, "%d %d %d %d %d %f %f %f %f %f\n", &dummy, &dummy, &dummy, &dummy, &dummy, &fdummy, &ebin_mean_map[i], &fdummy, &ebin_min_map[i], &ebin_max_map[i]);
	break;
      default:
	break;
      } 
    }
    ebin_min_map[0]=ebin_max_map[1];   
  }
  if (fclose(fp)) fprintf(stderr,"Error closing %s.",filename);
}

int fill_ebin_map_(){ /*fortran wrapper*/
  return(fill_ebin_map());
}

float ebin_mean(int ebin){
  return(ebin_mean_map[ebin]);
}

float ebin_min(int ebin){
  return(ebin_min_map[ebin]);
}

float ebin_max(int ebin){
  return(ebin_max_map[ebin]);
}

int fill_tbin_map(){
  char *dir = getenv("CLAS_PARMS");
  FILE *fp = NULL;
  int ret = 0;
  char filename[100], fname[100];
  int i;
  float tlow[61],toverlap[61],thigh[61];
  
  sprintf(filename, "%s/tagT-boundaries.dat", dir);
  if (fp = fopen(filename, "r")){
    for (i=1; i<=61; i++){
      int dummy;
      float fdummy;
      fscanf(fp, "%d %f %f %f\n", &dummy, &tlow[i], &toverlap[i], &thigh[i]);  
    }
  }
  if (fclose(fp)) fprintf(stderr,"Error closing %s.",filename);

  /* Assign boundaries to the 121 T-bins */

  tbin_min_map[121]=tlow[61];  
  for (i=61; i>=2; i--){
    tbin_max_map[2*i-1]=toverlap[i];
    tbin_min_map[2*i-2]=toverlap[i];
    tbin_max_map[2*i-2]=thigh[i];
    tbin_min_map[2*i-3]=thigh[i];
  }
  tbin_max_map[1]=thigh[1];
  
    
}


int fill_tbin_map_(){ /*fortran wrapper*/
  return(fill_tbin_map());
}

float tbin_min(int tbin){
  return(tbin_min_map[tbin]);
}

float tbin_max(int tbin){
  return(tbin_max_map[tbin]);
}


float ebin_calc_mass(int ebin, float beam_energy, float target_mass){
  vector4_t beam = {0.0, 0.0, 0.0, 0.0};
  vector4_t target = {0.0, 0.0, 0.0, 0.0};
  vector4_t sum;
  target.t = target_mass;
  beam.t = beam.space.z = beam_energy;
  sum = v4add(beam, target);
  return(v4mass(sum));
}


float ebin_mass(int ebin){
  vector4_t beam = {0.0, 0.0, 0.0, 0.0};
  clasRUNC_t *RUNC = getBank(&wcs_, "RUNC");
  if (RUNC){
    vector4_t sum;
    beam.t = beam.space.z = ebin_mean_map[ebin]*RUNC->runc.beam.p.t;
    sum = v4add(RUNC->runc.target, beam); 
    return (v4mass(sum));
  }
  return 0.0;
}

/* public E-T map member functions */
int fill_E_T_map(){
  char *dir = getenv("CLAS_PARMS");
  FILE *fp = NULL;
  int ret = 0;
  char filename[100], fname[100];
  int i, e_counter, tmin, tmax;
  int t_counter, emin, emax;


  sprintf(filename, "%s/tagETcoinc.dat", dir);
  if (fp = fopen(filename, "r")){
    for (i = 0; i < NUM_E_COUNTERS; i++){
      fscanf(fp, "%d %d %d \n", &e_counter, &tmin, &tmax);
      E_T_map[e_counter-1].n_tcounter = tmax - tmin + 1;
      E_T_map[e_counter-1].tmin = tmin;
      E_T_map[e_counter-1].tmax = tmax;
    } 
    ret = 1;
  }
  if (fclose(fp)) fprintf(stderr,"Error closing %s.",filename);
  sprintf(fname, "%s/tagTEcoinc.dat", dir);
  if (fp = fopen(fname, "r")){
    for (i = 0; i < NUM_T_COUNTERS; i++){
      fscanf(fp, "%d %d %d \n", &t_counter, &emin, &emax);
      T_E_map[t_counter-1].emin = emin;
      T_E_map[t_counter-1].emax = emax;
    } 
    ret = 1;
  } 
  if (fclose(fp)) fprintf(stderr,"Error closing %s.",fname);
  return ret;
}

int fill_e_t_map_(){
  return(fill_E_T_map());
}

/* The following routines all expect an index beginning from 1, therefore,
   subtract 1 from id to get index into the array */

int n_tcounter(int Eid){
  return (E_T_map[Eid-1].n_tcounter);
}

int tmin(int Eid){
  return (E_T_map[Eid-1].tmin);
}

int tmax(int Eid){
  return (E_T_map[Eid-1].tmax);
}

int emin(int Tid){
  return (T_E_map[Tid-1].emin);
}

int emax(int Tid){
  return (T_E_map[Tid-1].emax);
}

int E_T_matrix2arr(int matrix[NUM_E_BINS + 1][NUM_T_BINS +1], int *intarray){
  int i = 0;
  int e_bin, t_bin;
  int low_phys_id, high_phys_id, phys_id;

  for (e_bin = 1; e_bin <= NUM_E_BINS; e_bin++){
    for(t_bin = T_binmin(e_bin); t_bin <= T_binmax(e_bin); t_bin++){
	intarray[i++] = matrix[e_bin][t_bin];
    }
  }
}

int check_array_size(){
  int i = 0;
  int e_bin, t_bin;

  for (e_bin = 1; e_bin <= NUM_E_BINS; e_bin++){
    for(t_bin = T_binmin(e_bin); t_bin <= T_binmax(e_bin); t_bin++){
	i++;
    }
  }
  return(i);
}

int put_E_T_MATRIX_map(int runno, int matrix[NUM_E_BINS + 1][NUM_T_BINS +1]) {
  int i = 0;
  int e_bin, t_bin;
  int low_phys_id, high_phys_id, phys_id;
  char *dir = getenv("CLAS_PARMS");
  char map[100];
  int first;
  int intarray[6000];
  /* int intarray[E_T_ARRAY_SIZE]; */
  memset(intarray, 0, sizeof(int)*E_T_ARRAY_SIZE);
  sprintf(map, "%s/Maps/E_T_MATRIXtest.map", dir); 
  E_T_matrix2arr(matrix, intarray);
  map_put_int(map, "matrix", "contents", E_T_ARRAY_SIZE, intarray, runno); 
}

int arr2E_T_matrix(int *intarray, int matrix[NUM_E_BINS + 1][NUM_T_BINS +1]){
  int i = 0;
  int first, phys_id, t_bin, e_bin, low_phys_id, high_phys_id;

  for (e_bin = 1; e_bin <= NUM_E_BINS; e_bin++){  
    for(t_bin = T_binmin(e_bin); t_bin <= T_binmax(e_bin); t_bin++){
      matrix[e_bin][t_bin] = intarray[i++];
    }
  }
}

int get_E_T_matrix_map(int runno, int matrix[NUM_E_BINS+1][NUM_T_BINS+1]) {
  char *dir = getenv("CLAS_PARMS");
  char map[100];
  int first;
  int intarray[E_T_ARRAY_SIZE];
  int t_id;

  sprintf(map, "%s/Maps/E_T_MATRIX.map", dir);
  map_get_int(map, "matrix", "contents", E_T_ARRAY_SIZE, intarray, runno, &first);
  memset((void *)matrix, 0, sizeof(int)*(NUM_E_BINS+1)*(NUM_T_BINS+1));
  arr2E_T_matrix(intarray, matrix);
  
}

/* text file manipulation*/
int write_E_T_array(FILE *fp, int *array){
  int i;

  fprintf(fp, "Ebin  Tbin  Value\n");
  for(i = 0; i < E_T_ARRAY_SIZE; i++){
    int ebin, tbin;
    arrayind2matrix(i, &ebin, &tbin);
    fprintf(fp, "%3d  %3d  %7d\n", ebin, tbin, array[i]);
  }
  fprintf(fp, "\n");
}

int read_E_T_array(FILE *fp, int *array){
  int ebin, tbin, i;

  fscanf(fp, "Ebin  Tbin  Value\n");
  for(i = 0; i < E_T_ARRAY_SIZE; i++){
    fprintf(fp, "%3d  %3d  %7d\n", ebin, tbin, array[i]);
  }

}

int E_T_matrix2Tbin(int matrix[NUM_E_BINS+1][NUM_T_BINS+1], int *Tbin){
  int t_bin, e_bin;

  memset(Tbin, 0, sizeof(int)*(NUM_T_BINS+1));

  for(t_bin = 1; t_bin <= NUM_T_BINS; t_bin++){
    for(e_bin=1; e_bin <= NUM_E_BINS; e_bin++){
      Tbin[t_bin] += matrix[e_bin][t_bin];
    }
  }

}

int write_e_ijk_info(FILE *fp, int matrix[NUM_E_BINS+1][NUM_T_BINS+1]){
  int i, j, k;
  int Tid[NUM_T_COUNTERS+1];
  int Tbin[NUM_T_BINS+1];

  E_T_matrix2Tbin(matrix, Tbin);
  Tbin2Tid_arr(Tbin, Tid);
  fprintf(stdout, "   Tid  Ebin  Tbin E_T_M Tbin[] Tid[]  e_ijk\n");
  for(i = 1; i <=NUM_T_COUNTERS; i++){
    for(j=E_binmin(i);j<=E_binmax(i);j++){
      for(k=T2Tbinmin(i);k<=T2Tbinmax(i);k++){
	float epsilon=epsilon_ijk(matrix[j][k],Tid[i]);
	if (matrix2arrayind(j,k) != -1){ /* don't write out if illegal index!*/
	  fprintf(fp, "%5d %5d %5d %5d %5d %5d %10.5f\n",i,j,k,
		  matrix[j][k], Tbin[k], Tid[i], epsilon);
	}
      }
    }  
  }
  fprintf(fp, "\n");
}

int Tbin2Tid_arr(int Tbin[NUM_T_BINS+1], int Tid[NUM_T_COUNTERS+1]) {
  int j;
  
  memset(Tid, 0, sizeof(int)*62);
  Tid[1] = Tbin[1]+Tbin[2];
  for(j=2;j<=60;j++) {
    Tid[j] = Tbin[2*j-2]+Tbin[2*j-1]+Tbin[2*j];
  }
  Tid[61] = Tbin[120]+Tbin[121];
}  
int get_lbounds(int runno, float lbounds[NUM_E_BINS + 1]){
  int i;
  int first;
  float beam_e[1];
  char mapname[100];
  char *dir = getenv("CLAS_PARMS");

  sprintf(mapname, "%s/Maps/RUN_CONTROL.map", dir);
  map_get_float(mapname, "beam", "energy", 1, beam_e, runno, &first); 
  fill_ebin_map();
  for(i=1;i <= NUM_E_BINS;i++){
    lbounds[i] = ebin_min_map[NUM_E_BINS - i + 1] * beam_e[0]/1000;
  }
  lbounds[NUM_E_BINS + 1] = ebin_max_map[1] * beam_e[0]/1000;
}











