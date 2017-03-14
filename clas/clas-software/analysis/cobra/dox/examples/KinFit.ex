// run a kinematic fit for p,pi+,pi- detected 
vector<TLorentzVector> p4(3);

// assume we get the 4-momenta from somewhere and are set to p4p,p4pip,p4pim...
p4[0] = p4p;
p4[1] = p4pip;
p4[2] = p4pim;

// and the tagged photon energy...
double e_gamma = eg;

// and target mass...
double m_targ = mt;

// and the covariance matrix (see KFitUtils function GetCovMatrix)
TMatrixD cov(10,10) = covMatrix;

// set up a KinFit object for this event
KinFit kfit;
kfit.SetEvent(e_gamma,p4,cov,m_targ);

// run a fit to a missing pi0
kfit.Fit("pi0");

// run a fit to nothing missing
kfit.Fit();

// etc....

// set kinematic quantities to the output of the fit (if it's a good fit)
if(kfit.Prob() > 0.1){
  // confidence level cut of 10%
  e_gamma = kfit.FitPhotonEnergy();
  for(int i = 0; i < 3; i++) p4[i] = kfit.FitP4(i);
}
