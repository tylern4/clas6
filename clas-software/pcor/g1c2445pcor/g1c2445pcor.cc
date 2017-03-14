#include <fstream>
#include <cmath>
#include "g1c2445pcor.h"

//_____________________________________________________________________________

// Function prototypes:
int Getg1c2445Sector(const float __p3[3]);

int Getg1c2445Bin(double __theta,double __phi,const double __phiBins_min[12],
    const double __phiBins_max[12],const double __thetaBins_min[15],
    const double __thetaBins_max[15]);			

void Readg1c2445BinsFile(double __phiBins_min[12],double __phiBins_max[12],
    double __thetaBins_min[15],double __thetaBins_max[15]);

double Getg1c2445LambdaTrack(const float __p3[3]);

double Getg1c2445PhiTrack(const float __p3[3]);

void Setg1c2445P3FromTrackingParameters(double __p,double __lambda,double __phi,
    int __sector,float __p3[3]);

double Getg1c2445SectorPhi(double __phi);

void Init_g1c2445pcor(double __p_vals[2][6][180][5],int __num_pars_p[2][6][180],
    double __phi_vals[2][6][180][4],
    int __num_pars_phi[2][6][180],
    double __lambda_vals[2][6][180][4],
    int __num_pars_lambda[2][6][180]);
//_____________________________________________________________________________

// FORTRAN wrappers:

void g1c2445TaggerCor_(float *__eg,float *__e_beam,int *__eid,int *__runNumber,
    float *__egCor){
  *__egCor = g1c2445TaggerCor(*__eg,*__e_beam,*__eid,*__runNumber);
}

void g1c2445MomCor_(float __p3[3],int *__q,float __p3cor[3]){
  g1c2445MomCor(__p3,*__q,__p3cor);
}

void g1c2445FiducialCut_(float __p3[3],int *__q,int *__cut){
  bool is_good = g1c2445FiducialCut(__p3,*__q);
  if(is_good) *__cut = 1;
  else *__cut = 0;
}
//_____________________________________________________________________________

/// Tagger correction for the g1c2445 run period
/** @param eg         Measured photon energy (GeV)
 *  @param e_beam     Beam energy (GeV)
 *  @param eid        E-paddle id in the tagger
 *  @param runNumber  CLAS run number
 *
 *  <b>Returns</b>    Corrected photon energy
 *
 *  Corrects the tagger measurement using parameters from tagger_cor.dat (to 
 *  find this file, the user's CLAS_PACK enviornment variable must be set).
 *  Next a run dependent beam energy correction is applied. The corrected
 *  energy is returned.
 *
 *  Note: If the correction parameter file can not be opened (most likely due
 *        to inproper enviornment set up), then -666 is returned.
 *
 */
float g1c2445TaggerCor(float __eg,float __e_beam,int __eid,int __runNumber){

  static bool first_call = true;
  static float eid_cor[767];
  static float offset[700];

  if(first_call){ // only do this on the 1st call to the function
    // read in the correction parameters
    char file[200];
    sprintf(file,"%s/utilities/g1c2445pcor/tagger_cor.dat",getenv("CLAS_PACK"));
    std::ifstream inFile_egcor(file);
    // check to make sure the file opened properly
    if(!inFile_egcor.is_open()){
      printf("Error!!!! <g1c2445TaggerCor> Can NOT open tagger_cor.dat. This ");
      printf("file should be located in CLAS_PACK/utilites/g1c2445pcor. Check ");
      printf("to make sure your CLAS_PACK enviornmen variable is properly ");
      printf("set. \n");
      return -666.; // return nonsense
    }

    int eid,count = 0;;
    float cor;
    while(inFile_egcor >> eid){
      inFile_egcor >> cor;
      eid_cor[eid-1] = cor;
      count++;
    }    
    inFile_egcor.close(); // close the input file 
    if(count != 767){
      printf("Error!!!! <g1c2445TaggerCor> Read incorrect number of E-paddles ");
      printf("[%d instead of 767]. \n",count);
      return -666.;
    }
    // read in beam offsets
    sprintf(file,"%s/utilities/g1c2445pcor/beam_offset.dat",getenv("CLAS_PACK"));
    std::ifstream inFile_offset(file);
    int run;
    float beam_off;
    for(int i = 0; i < 700; i++) offset[i] = 0.;

    while(inFile_offset >> run){
      inFile_offset >> beam_off;
      offset[run - 43490] = beam_off;
    }
    inFile_offset.close(); // close the file
    first_call = false;
  }

  // Now apply the correction for this event

  // check that eid is valid
  if(__eid < 1 || __eid > 767){
    printf("Warning <g1c2445TaggerCor> E-paddle out of range [id = %d].",__eid);
    printf(" No correction will be applied. \n");
    return __eg;
  }

  float dEg = __e_beam*eid_cor[__eid-1]; // tagger correction
  dEg += offset[__runNumber - 43490]; // add beam correction if we have one

  return __eg + dEg; // return the corrected energy
}
//_____________________________________________________________________________

/// Momentum corrections for g1c2445.
/** @param p3  Measured 3-momentum (x,y,z)
 *  @param q   Charge in units of @a e (ex. -1 for pi-)
 *  @param p3cor Corrected 3-momentum
 *
 *  Sets p3cor to be the corrected momentum.
 */
void g1c2445MomCor(float __p3[3],int __q,float __p3cor[3]){

  static bool firstCall = true;
  static double phiBins_min[12],phiBins_max[12];
  static double thetaBins_min[15],thetaBins_max[15];
  const int num_bins = 180;
  static double p_vals[2][6][180][5];
  static double phi_vals[2][6][180][4];
  static double lambda_vals[2][6][180][4];
  static int num_pars_p[2][6][180];
  static int num_pars_lambda[2][6][180];
  static int num_pars_phi[2][6][180];
  int bin;

  if(firstCall){ // on first call, do the following...
    // read in binning info
    Readg1c2445BinsFile(phiBins_min,phiBins_max,thetaBins_min,thetaBins_max);
    // read in momcor parameters
    Init_g1c2445pcor(p_vals,num_pars_p,phi_vals,num_pars_phi,lambda_vals,
        num_pars_lambda);
    firstCall = false;
  }

  // Get the kinematic quantities we need
  double px = __p3[0],py = __p3[1],pz = __p3[2];
  double p = sqrt(px*px + py*py + pz*pz),p_cor;
  double theta_lab = atan2(sqrt(px*px + py*py),pz);
  double phi_lab = atan2(py,px);
  double lam = Getg1c2445LambdaTrack(__p3),lam_cor;
  double phi = Getg1c2445PhiTrack(__p3),phi_cor;
  double par[4];
  int sector,q_index;
  sector = Getg1c2445Sector(__p3);

  if(__q < 0) q_index = 1;
  else q_index = 0;

  bin = Getg1c2445Bin(theta_lab,phi_lab,phiBins_min,phiBins_max,thetaBins_min,
      thetaBins_max);

  if(bin < 0) {// no correction for this (theta,phi)
    for(int i = 0; i < 3; i++) __p3cor[i] = __p3[i];
    return;
  }

  // correct |p|
  double p_scale = p;
  if(p > 2.) p_scale = p;
  for(int i = 0; i < 5; i++) par[i] = p_vals[q_index][sector-1][bin][i];

  if(num_pars_p[q_index][sector-1][bin] == 2)
    p_cor = p + p_scale*(par[0]*p + par[1]);
  else if(num_pars_p[q_index][sector-1][bin] == 3)
    p_cor = p + p_scale*(par[0]*p*p + par[1]*p + par[2]);
  else if(num_pars_p[q_index][sector-1][bin] == 4) {
    p_cor = p + p_scale*(par[3]*p*p*p+par[2]*p*p+par[1]*p+par[0]);
  }
  else p_cor = p;

  // correct tracking angle lambda
  for(int i = 0; i < 4; i++) par[i] = lambda_vals[q_index][sector-1][bin][i];
  if(num_pars_lambda[q_index][sector-1][bin] == 4)
    lam_cor = lam + (par[0]/(p*p*p) + par[1]/(p*p) + par[2]/p + par[3]);
  else if(num_pars_lambda[q_index][sector-1][bin] == 2)
    lam_cor = lam + (par[0]*p + par[1]);
  else lam_cor = lam;

  // correct tracking angle phi
  for(int i = 0; i < 4; i++) par[i] = phi_vals[q_index][sector-1][bin][i];
  if(num_pars_phi[q_index][sector-1][bin] == 4)
    phi_cor = phi + (par[0]/(p*p*p) + par[1]/(p*p) + par[2]/p + par[3]);
  else if(num_pars_phi[q_index][sector-1][bin] == 2)
    phi_cor = phi + (par[0]*p + par[1]);
  else phi_cor = phi;

  //this two just temporary while I just do corrections to the momentum, 
  //I don't want to correct angles quite yet.
  lam_cor = lam;
  phi_cor = phi; 

  // set corrected 3-momenta
  Setg1c2445P3FromTrackingParameters(p_cor,lam_cor,phi_cor,sector,__p3cor);  
}
//_____________________________________________________________________________

bool g1c2445FiducialCut(float __p3[3],int __q){

  double px = __p3[0],py = __p3[1],pz = __p3[2];
  double p = sqrt(px*px + py*py + pz*pz);
  double theta = atan2(sqrt(px*px + py*py),pz);
  double phi = atan2(py,px);
  theta *= 180./3.14159; // raidans --> degrees
  phi = Getg1c2445SectorPhi(phi)*180./3.14159; // sector phi in degrees
  if(__q > 0){
    if(theta <= 10.){
      if(std::abs(phi) >= 10.) return false;
    }
    else if(theta <= 15.){
      if(std::abs(phi) >= 15.) return false;
    }
    else if(theta <= 35.){
      if(std::abs(phi) >= 20.) return false;
    }
    else if(std::abs(phi) >= 25.) return false;
  }
  else{
    if(theta <= 10.) return false;
    else if(theta <= 15.){
      if(std::abs(phi) >= 10.) return false;
    }
    else if(theta <= 20.){
      if(std::abs(phi) >= 15.) return false;
    }
    else if(theta <= 35.){
      if(std::abs(phi) >= 20.) return false;
    }
    else if(std::abs(phi) >= 25.) return false;
  }
  return true;
}
//_____________________________________________________________________________

/// Reads the binning.dat file to get (theta,phi) bins
void Readg1c2445BinsFile(double __phiBins_min[12],double __phiBins_max[12],
    double __thetaBins_min[15],double __thetaBins_max[15]){

  char file[200];
  sprintf(file,"%s/utilities/g1c2445pcor/binning.dat",getenv("CLAS_PACK"));
  std::ifstream binsFile(file);
  int num_bins;

  if(!binsFile.is_open()){ // couldn't open the file
    printf("Error!!! <Readg1c2445BinsFile> Could NOT open binning.dat. ");
    printf("This function is called by g1c2445MomCor and should be located in ");
    printf("CLAS_PACK/utilities/g1c2445pcor, make sure your CLAS_PACK ");
    printf("enviornment variable is set properly. \n");
    abort();
  }

  std::string str;
  // skip over the commented lines (lines that start with #)
  while(binsFile >> str){
    if(str[0] != '#') break;
    else binsFile.ignore(1000,'\n');
  }

  // get phi bins
  num_bins = atoi(str.c_str());
  if(num_bins != 12){ 
    printf("Error!!! <Readg1c2445BinsFile> Read incorrect number of phi bins ");
    printf("[%d instead of 12].",num_bins);
    printf("This function is called by g1c2445MomCor and should be located in ");
    printf("CLAS_PACK/utilities/g1c2445pcor, make sure your CLAS_PACK ");
    printf("enviornment variable is set properly. \n");
    abort();
  }
  for(int i = 0; i < num_bins; i++) 
    binsFile >> __phiBins_min[i] >> __phiBins_max[i];

  // skip next block of commented lines
  while(binsFile >> str){
    if(str[0] != '#') break;
    else binsFile.ignore(1000,'\n');
  }

  // get theta bins
  num_bins = atoi(str.c_str());
  if(num_bins != 15){ 
    printf("Error!!! <Readg1c2445BinsFile> Read incorrect number of theta bins ");
    printf("[%d instead of 15].",num_bins);
    printf("This function is called by g1c2445MomCor and should be located in ");
    printf("CLAS_PACK/utilities/g1c2445pcor, make sure your CLAS_PACK ");
    printf("enviornment variable is set properly. \n");
    abort();
  }

  for(int i = 0; i < num_bins; i++) 
    binsFile >> __thetaBins_min[i] >> __thetaBins_max[i];

  binsFile.close(); // close the file
}
//_____________________________________________________________________________

/// Gets CLAS sector number from 3-momentum
int Getg1c2445Sector(const float __p3[3]){

  int sector = 0;
  double px = __p3[0],py = __p3[1],pz = __p3[2];
  double pi = 3.14159;
  double phi_lab = atan2(py,px);
  double phi = (180./pi)*phi_lab; // radians --> degrees

  if(std::abs(phi) <= 30.) sector = 1;
  else if(phi > 0.){
    if(phi <= 90.) sector = 2;
    else if(phi <= 150) sector = 3;
    else sector = 4;
  }
  else {
    // phi < 0
    if(std::abs(phi) <= 90.) sector = 6;
    else if(std::abs(phi) <= 150.) sector = 5;
    else sector = 4;
  }
  return sector;
}
//_____________________________________________________________________________

/// Getg1c2445 (theta,phi) bin
int Getg1c2445Bin(double __theta,double __phi,const double __phiBins_min[12],
    const double __phiBins_max[12],const double __thetaBins_min[15],
    const double __thetaBins_max[15]){

  double sec_phi = Getg1c2445SectorPhi(__phi);
  sec_phi *= 180./3.14159; // convert to degrees
  double theta = __theta * 180./3.14159; // convert to degrees

  int phi_bin = -1;
  for(int i = 0; i < 12; i++){
    if(sec_phi >= __phiBins_min[i] && sec_phi < __phiBins_max[i]){
      phi_bin = i;
      break;
    }
  }
  if(phi_bin == -1) return -1;

  int theta_bin = -1;
  for(int i = 0; i < 15; i++){
    if(theta >= __thetaBins_min[i] && theta < __thetaBins_max[i]){
      theta_bin = i;
      break;
    }
  }
  if(theta_bin == -1) return -1;

  return 12*theta_bin + phi_bin;  
}
//_____________________________________________________________________________

/// Calculates the tracking angle \f$ \lambda \f$.
double Getg1c2445LambdaTrack(const float __p3[3]){

  double lambda;
  double px = __p3[0],py = __p3[1],pz = __p3[2];
  double p_mag = sqrt(px*px + py*py + pz*pz);
  double x = px/p_mag,y = py/p_mag;

  double alpha = (3.14159/3.)*(Getg1c2445Sector(__p3)-1);
  lambda = asin(cos(alpha)*y - sin(alpha)*x);
  return lambda;
}
//_____________________________________________________________________________

/// Calculates the tracking angle \f$ \phi \f$.
double Getg1c2445PhiTrack(const float __p3[3]){

  double phi;
  double px = __p3[0],py = __p3[1],pz = __p3[2];
  double z = pz/sqrt(px*px + py*py + pz*pz); // normalized z_lab
  double lambda = Getg1c2445LambdaTrack(__p3);

  phi = acos(z/cos(lambda));
  return phi;
}
//_____________________________________________________________________________

void Setg1c2445P3FromTrackingParameters(double __p,double __lam,double __phi,
    int __sector,float __p3[3]){

  double alpha = (3.14159/3.)*(__sector - 1);
  __p3[0] = __p*(cos(__lam)*sin(__phi)*cos(alpha) - sin(__lam)*sin(alpha));
  __p3[1] = __p*(cos(__lam)*sin(__phi)*sin(alpha) + sin(__lam)*cos(alpha));
  __p3[2] = __p*cos(__lam)*cos(__phi);    
}
//_____________________________________________________________________________

double Getg1c2445SectorPhi(double __phi){

  double sec_phi = __phi;
  int sign = 1;
  if(__phi < 0) sign = -1;
  if(std::abs(sec_phi) < 3.14159/6.) return __phi;
  else{
    sec_phi -= sign*3.14159/3.;
    return Getg1c2445SectorPhi(sec_phi);
  }
}
//_____________________________________________________________________________

/// Reads in the momentum correction parameters
void Init_g1c2445pcor(double __p_vals[2][6][180][5],int __num_pars_p[2][6][180],
    double __phi_vals[2][6][180][4],
    int __num_pars_phi[2][6][180],
    double __lambda_vals[2][6][180][4],
    int __num_pars_lambda[2][6][180]){

  int bin,num_pars;
  std::ifstream *inFile = 0;
  double x[10];
  std::string file_base = getenv("CLAS_PACK");
  file_base += "/utilities/g1c2445pcor/pars_";
  char fileName[200];
  std::string charge_ext[2];
  charge_ext[0] = "pos";
  charge_ext[1] = "neg";

  for(int s = 1; s <= 6; s++){ // loop over sectors
    for(int qi = 0; qi < 2; qi++){ // loop over charges

      // get |p| parameters    
      sprintf(fileName,"%sp_sector%d.%s",file_base.c_str(),s,
          charge_ext[qi].c_str());
      inFile = new std::ifstream(fileName);
      if(!(inFile->is_open())){ // check that file was successfully opened
        printf("Error!!!! <Init_g1c2445pcor> Could NOT open %s. ",fileName);
        printf("This function is called by g1c2445MomCor to read in the ");
        printf("correction parameters. This function looks for files in ");
        printf("CLAS_PACK/utilities/g1c2445pcor, make sure your CLAS_PACK ");
        printf("enviornment variable is set correctly. \n");	
        abort();
      }

      while(*inFile >> bin){
        *inFile >> num_pars;
        __num_pars_p[qi][s-1][bin] = num_pars;
        for(int i = 0; i < num_pars; i++) {
          *inFile >> x[i];
        }
        for(int i = 0; i < 5; i++)
        {
          __p_vals[qi][s-1][bin][i] = 0.;
          if(i < num_pars) 
          {
            __p_vals[qi][s-1][bin][i] = x[i];
          }
        }
      }
      delete inFile; inFile = 0; // close the file

      // get lambda parameters
      sprintf(fileName,"%slambda_sector%d.%s",file_base.c_str(),s,
          charge_ext[qi].c_str());
      inFile = new std::ifstream(fileName);
      if(!(inFile->is_open())){
        printf("Error!!!! <Init_g1c2445pcor> Could NOT open %s. ",fileName);
        printf("This function is called by g1c2445MomCor to read in the ");
        printf("correction parameters. This function looks for files in ");
        printf("CLAS_PACK/utilities/g1c2445pcor, make sure your CLAS_PACK ");
        printf("enviornment variable is set correctly. \n");	
        abort();
      }

      while(*inFile >> bin){
        *inFile >> num_pars;
        __num_pars_lambda[qi][s-1][bin] = num_pars;
        for(int i = 0; i < num_pars; i++) *inFile >> x[i];
        for(int i = 0; i < 4; i++){
          __lambda_vals[qi][s-1][bin][i] = 0.;
          if(i < num_pars) __lambda_vals[qi][s-1][bin][i] = x[i];
        }
      }
      delete inFile; inFile = 0; // close the file

      // get phi parameters
      sprintf(fileName,"%sphi_sector%d.%s",file_base.c_str(),s,
          charge_ext[qi].c_str());
      inFile = new std::ifstream(fileName);
      if(!(inFile->is_open())){
        printf("Error!!!! <Init_g1c2445pcor> Could NOT open %s. ",fileName);
        printf("This function is called by g1c2445MomCor to read in the ");
        printf("correction parameters. This function looks for files in ");
        printf("CLAS_PACK/utilities/g1c2445pcor, make sure your CLAS_PACK ");
        printf("enviornment variable is set correctly. \n");
        abort();
      }
      
      while(*inFile >> bin){
        *inFile >> num_pars;
        __num_pars_phi[qi][s-1][bin] = num_pars;
        for(int i = 0; i < num_pars; i++) *inFile >> x[i];	  
        for(int i = 0; i < 4; i++){
          __phi_vals[qi][s-1][bin][i] = 0.;
          if(i < num_pars) __phi_vals[qi][s-1][bin][i] = x[i];
        }
      }
      delete inFile; inFile = 0; // close the file

    }
  }

}
//_____________________________________________________________________________
