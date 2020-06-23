#include "KFitUtils.h"
#include <cmath>
/** @file KFitUtils.C
 * @brief Kinematic fitting utility function source file.
 */
//_____________________________________________________________________________

/// Calculates the tracking quantity \f$ \alpha = \frac{\pi}{3}(sector - 1) \f$
double AlphaTrack(const TLorentzVector &__p4){

  int sector = 0;
  double pi = 3.14159;
  double phi_lab = __p4.Phi();
  double phi = (180./pi)*phi_lab;

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
  return (pi/3.)*(sector - 1);
}
//_____________________________________________________________________________

/// Calculates the tracking angle \f$ \lambda \f$.
double LambdaTrack(const TLorentzVector &__p4){

  double lambda;
  double p_mag = __p4.P();
  double x = __p4.X()/p_mag,y = __p4.Y()/p_mag;
  
  double alpha = AlphaTrack(__p4);
  lambda = asin(cos(alpha)*y - sin(alpha)*x);
  return lambda;
}
//_____________________________________________________________________________

/// Calculates the tracking angle \f$ \phi \f$.
double PhiTrack(const TLorentzVector &__p4) {
  
  double phi;
  double z = __p4.Pz()/__p4.P(); // normalized z_lab
  double lambda = LambdaTrack(__p4);

  phi = acos(z/cos(lambda));
  return phi;
}
//_____________________________________________________________________________

/// Calculates and returns the covariance matrix given the tracking matrix.
/**
 * @param covTrack Tracking covariance matrix
 * @param p4 Vector of detected particle 4-momenta
 * @param vert Vector of detected particle verticies
 * @param runNum CLAS run number
 * @param momCor Were momentum corrections performed?
 * @param isMC Is this monte carlo?
 *
 * This function calculates the full covariance matrix given the tracking
 * covariance matrix output by @a CLAS.
 */
TMatrixD GetCovMatrix(const TMatrixD &__covTrack,
		      const std::vector<TLorentzVector> &__p4,
		      const std::vector<TVector3> &__vert,int __runNum,
		      bool __momCor,bool __isMC){

  TMatrixD cov(__covTrack); // start it off as the tracking covariance matrix

  // covTrack must be square
  if(__covTrack.GetNrows() != __covTrack.GetNcols()){
    std::cout << "Error! <GetCovMatrix> Tracking covariance matrix passed to "
	      << "this function is NOT square. Size is " 
	      << __covTrack.GetNrows() << " x " << __covTrack.GetNcols()
	      << std::endl;
    abort();
  }

  // check that the sizes of covTrack and p4 are consistent
  int numParts = (int)__p4.size();  
  if(__covTrack.GetNrows() != (3*numParts + 1)){
    std::cout << "Error! <GetCovMatrix> Tracking covariance matrix size is NOT"
	      << " consistent with number of particle 4-momenta passed in. "
	      << numParts << " particles given so covariance matrix should be "
	      << (3*numParts+1) << " x " << (3*numParts+1) << " but has "
	      << __covTrack.GetNrows() << " rows and columns." << std::endl;
    abort();
  }
    
  static bool first_call = true;
  static double pPars[2][6][15][3],lamPars[2][6][15],phiPars[2][6][15];
  if(first_call){
    // initialize the parameter arrays
    ReadInResParams(pPars,lamPars,phiPars);
    first_call = false;
  }

  // get run dependent quantities
  const ClasRuns *clasRuns = ClasRuns::Instance();
  // tagged photon resolution
  double e_gamma_res = clasRuns->GetRunPeriod(__runNum).BeamEnergy();
  e_gamma_res *= 0.001/sqrt(3.); // convert energy to resolution

  double p_res_scale,sigma_eloss,lam_sig_ms,phi_sig_ms,p_offset;
  int part_index,sector,bin;
  double res_scale = 1.5;
  if(__isMC) res_scale *= 1.4;

  // set up the covariance matrix
  cov(0,0) = pow(e_gamma_res,2); // tagged photon sigma^2

  double beta,theta,p_mag,gamma;
  for(int i = 0; i < numParts; i++){      
    beta = __p4[i].Beta();
    gamma = __p4[i].Gamma();
    theta = __p4[i].Theta();
    p_mag = __p4[i].P();
    if(__p4[i].M() < .7) part_index = 1; // use pion parameters
    else part_index = 0; // use proton parameters
    
    sector = GetSectorFromP4(__p4[i]);
    bin = GetThetaBin(theta);
    
    // there weren't enough statistics to calculate proton resolution for lab
    // angles > 70 degrees...so if theta > 70 degrees, we'll just use the 
    // parameters for the 70 degree bin.
    if(part_index  == 0 && bin > 10) bin = 10;

    // get resolution/eloss parameters
    p_res_scale = pPars[part_index][sector-1][bin][0];
    sigma_eloss = pPars[part_index][sector-1][bin][1];
    if(__isMC) p_res_scale *= 1.4;

    // parameters in the files were obtained by fitting dP vs P in numerous
    // theta bins...in a few small kinematic regions, for the proton, small
    // corrections to these numbers are needed due to a poor fit,etc. 
    if(part_index == 0 && p_mag > 2.) p_res_scale *= 1.25;
    if(part_index == 0){
      if(p_mag > 0.4) sigma_eloss *= 0.8;
      else if(p_mag < 0.3) sigma_eloss *= 1.25;
    }

    // scale resolution errors
    cov(3*i+1,3*i+1) *= pow(p_res_scale,2);

    // add in eloss errors
    if(beta < .765)
      cov(3*i+1,3*i+1) += pow(sigma_eloss*gamma/beta,2)*(1.-beta*beta/2.);
    else
      cov(3*i+1,3*i+1) += pow(sigma_eloss/.765,2)
	*(1.-.765*.765/2.)/(1.-.765*.765);

    p_offset = pPars[part_index][sector-1][bin][2];
    cov(3*i+1,3*i+1) += p_offset*p_offset + 2*p_offset*sqrt(cov(3*i+1,3*i+1));
    

    // scale angular resolution errors
    lam_sig_ms = lamPars[part_index][sector-1][bin];
    phi_sig_ms = phiPars[part_index][sector-1][bin];

    cov(3*i+2,3*i+2) *= pow(res_scale,2);
    cov(3*i+3,3*i+3) *= pow(res_scale,2);

    /* add in multiple scattering errors */
    cov(3*i+2,3*i+2) += pow(lam_sig_ms/(p_mag*beta),2);
    cov(3*i+3,3*i+3) += pow(phi_sig_ms/(p_mag*beta),2);

    // scale off diagnol elements by resolution scale factors
    cov(3*i+1,3*i+2) *= res_scale*p_res_scale; 
    cov(3*i+2,3*i+1) = cov(3*i+1,3*i+2);
    cov(3*i+1,3*i+3) *= res_scale*p_res_scale; 
    cov(3*i+3,3*i+1) = cov(3*i+1,3*i+3);
    cov(3*i+2,3*i+3) *= res_scale*res_scale;
    cov(3*i+3,3*i+2) = cov(3*i+2,3*i+3);
  }

  return cov;
}
//_____________________________________________________________________________

/// Reads in the CLAS resolution parameters
bool ReadInResParams(double __pPars[2][6][15][3],double __lamPars[2][6][15],
		     double __phiPars[2][6][15]){

  std::ifstream *inFile = 0;
  String respars_loc_base = ("NO 'RESPARS_LOC' VARIABLE");
  if (getenv("RESPARS_LOC") == NULL) {
    std::cout << std::endl << "============= You need to add an environmental variable to your .cshrc file called RESPARS_LOC =============" << std::endl;
    std::cout << "=============           This variable points to the location of your respars arrays            =============" << std::endl << std::endl;
  }
  else {
    respars_loc_base = getenv("RESPARS_LOC");
  }
  respars_loc_base += "/";
  String fileName_base = getenv("COBRASYS");
  fileName_base += "/kinfit/";
  String fileName;
  String p_names[2];
  p_names[0] = "p";
  p_names[1] = "pi";
  int num_rows_read,bin,sector;
  double par[3];

  for(int part = 0; part < 2; part++){
    // open |p| pars files
    fileName = respars_loc_base + "respars_p." + p_names[part];
    inFile = new std::ifstream(fileName.c_str());

    if(!(inFile->is_open())){
      // file didn't open
      if (part == 0 && getenv("RESPARS_LOC") != NULL) {
	std::cout << std::endl << "                    ***NOTICE***" << std::endl;
	std::cout << "Either there have not been covariance matrix errors determined or they are in the wrong place!!" << std::endl;
	std::cout << "If they have been determined, check the value of env variable RESPARS_LOC (should be the location of your respars arrays)!!" << std::endl << std::endl;
      }

      std::cout << "Error! <ReadInResParams> File read error: " << fileName
		<< " Unable to open file." << std::endl;

      fileName = fileName_base + "respars_p." + p_names[part]; //getting the default (g11a) errors
      inFile = new std::ifstream(fileName.c_str());
      std::cout << "Now using default (g11a) respars ---> " << fileName << std::endl;
      if(!(inFile->is_open())){
	// file didn't open
	std::cout << "Error! <ReadInResParams> File read error: " << fileName
		  << " Unable to open file." << std::endl;
	return false;
      }
    }
    else {
      std::cout << "respars being used ---> " << fileName << std::endl;
    }
    
    num_rows_read = 0;
    while(*inFile >> sector){
      num_rows_read++;
      *inFile >> bin >> par[0] >> par[1] >> par[2];
      for(int i = 0; i < 3; i++) __pPars[part][sector-1][bin][i] = par[i];
    }

    // check correct number of rows were read in
    if(!CheckRowsRead(fileName,num_rows_read)) return false;
    delete inFile; inFile = 0;

    // read in lambda tracking angle pars
    fileName = respars_loc_base + "respars_lambda." + p_names[part];
    inFile = new std::ifstream(fileName.c_str());

    if(!(inFile->is_open())){
      // file didn't open
      std::cout << "Error! <ReadInResParams> File read error: " << fileName
		<< " Unable to open file." << std::endl;
      
      fileName = fileName_base + "respars_lambda." + p_names[part]; //getting the default (g11a) errors
      inFile = new std::ifstream(fileName.c_str());
      std::cout << "Now using default (g11a) respars ---> " << fileName << std::endl;

      if(!(inFile->is_open())){
	// file didn't open
	std::cout << "Error! <ReadInResParams> File read error: " << fileName
		  << " Unable to open file." << std::endl;
	return false;
      }
    }
    else {
      std::cout << "respars being used ---> " << fileName << std::endl;
    }
    
    num_rows_read = 0;
    while(*inFile >> sector){
      num_rows_read++;
      *inFile >> bin >> par[0];
      __lamPars[part][sector-1][bin] = par[0];
    }
  
    // check correct number of rows were read in
    if(!CheckRowsRead(fileName,num_rows_read)) return false;
    delete inFile; inFile = 0;

    // read in phi tracking angle pars
    fileName = respars_loc_base + "respars_phi." + p_names[part];
    inFile = new std::ifstream(fileName.c_str());

    if(!(inFile->is_open())){ // file didn't open
      std::cout << "Error! <ReadInResParams> File read error: " << fileName
		<< " Unable to open file." << std::endl;
      
      fileName = fileName_base + "respars_phi." + p_names[part]; //getting the default (g11a) errors
      inFile = new std::ifstream(fileName.c_str());
      std::cout << "Now using default (g11a) respars ---> " << fileName << std::endl;

      if(!(inFile->is_open())){
	// file didn't open
	std::cout << "Error! <ReadInResParams> File read error: " << fileName
		  << " Unable to open file." << std::endl;
	return false;
      }
    }
    else {
      std::cout << "respars being used ---> " << fileName << std::endl;
    }
    
    num_rows_read = 0;
    while(*inFile >> sector){
      num_rows_read++;
      *inFile >> bin >> par[0];
      __phiPars[part][sector-1][bin] = par[0];
    }

    // check correct number of rows were read in
    if(!CheckRowsRead(fileName,num_rows_read)) return false;
    delete inFile; inFile = 0;
  }

  return true; // if we get here, everything should be ok
}
//_____________________________________________________________________________
