#include "ClasEvent.h"
/** @file ClasEvent.C
 * @brief ClasEvent class source file.
 */
//_____________________________________________________________________________
/** @class ClasEvent
 *  @brief Main class used to filter channels out of @a CLAS data files.
 *
 *  ClasEvent is the main class for use in selecting events out of @a CLAS data
 *  files. It inherits from Event, thus once an event is loaded it has all of
 *  Event's kinematic functions. In addition to those, ClasEvent has functions
 *  for performing momentum corrections, eloss corrections and kinematic 
 *  fitting.
 *
 *  ClasEvent provides interfaces to the various @a CLAS data file formats.
 *  Each of these must define an instantiation of ClasEvent::SetClasEvent and
 *  ClasEvent::GetEvent. See these functions for examples of how to get an 
 *  event loaded into a ClasEvent object for each interface.
 *
 *  Currently, 2 interfaces are provided. One for BOS files and one for the
 *  compressed ROOT files. In cobra/tutorials, the files 
 *  <a class="blue" href="SkimOmega_8cc-source.html">SkimOmega.cc</a> and 
 *  <a class="blue" href="BosSkimOmega_8cc-source.html">BosSkimOmega.cc</a>
 *  provide examples of how to skim the same channel 
 *  (\f$\gamma p \rightarrow p \omega \rightarrow p \pi^+ \pi^- (\pi^0)\f$)
 *  from compressed ROOT files and BOS files respectively.
 *
 *  Both of the above files produce @a Event files. These are ROOT files which 
 *  have a TTree with a branch containing Event objects for each event that
 *  passes the skim.  Once @a Event files are produced, then all subsequent
 *  analysis can be done without any linking to CLAS software and, when
 *  appropriate, in an interactive ROOT session.
 *
 */
//_____________________________________________________________________________

ClasEvent::ClasEvent(const String &__p_names):Event(__p_names){
  /// Constructor
  /** 
   * @param p_names Particle names string of format <em>"a:b:c:..."</em>
   *
   * The ctor parses @a p_names to get the names of the particles that are
   * going to be required to be detected. The format of the string is
   * <em>"name1:name2:name3:..."</em> for as many names are needed. See
   * ParticleTypes for information pertaining to valid particle names (for
   * a quick reference see ListOfParticles.lst). 
   *
   * Calls Index::Init and sets the id scheme to @a part and sets the default
   * timing cut (photon vertex time/event start time) to be 1.5 ns (see
   * Index::SetTimingCut). 
   *
   * <b>Examples </b>
   *
   * \include ClasEvent_Ctor.ex 
   */
  int num = _particles.Size(); // number of particles found in p_names
  _momentumCor = false;
  _isMC = false;
  // initialize the Index object
  std::vector<String> names;
  for(int i = 0; i < num; i++) names.push_back(_particles[i].Name());
  _index.Init(names);

  // set default id to be part (PART bank id scheme)
  _index.SetIdScheme("part");
  _index.SetTimingCut(1.5); // default timing cut to 1.5 ns
    
  // initialize the tracking covariance matrix
  int dim = 3*_particles.Size()+1;
  _covMatTrack.ResizeTo(dim,dim);
  _covMatTrack.Zero(); 
}   
//_____________________________________________________________________________

ClasEvent& ClasEvent::operator=(const ClasEvent &__ce){
  /// Assignment operator
  this->Event::operator=(__ce);
  this->_Copy(__ce);

  return *this;
}
//_____________________________________________________________________________

void ClasEvent::PrintConfig() const {
  /// Print current ClasEvent configuration.
  std::cout << "ClasEvent Configuration: ";
  std::string targ = "p";
  if(this->TargetMass() > 1) targ = "d";
  std::cout << "g " << targ << " --> ";
  for(int i = 0; i < _particles.Size(); i++) {
    std::cout << _particles[i].Name() << " ";
  }
  std::cout << "(X) [" << _index.IdScheme() << " id]" << std::endl;
}   
//_____________________________________________________________________________

void ClasEvent::MomCor(){
  /// Perform energy loss corrections on all detected charged particles.
  /** Runs InitEloss any time the target material or cell change (also runs
   *  it the first time it's called).
   */
  TLorentzVector p4cor;
  int targMat = this->GetRunPeriod().TargetMaterialFlag();
  int targCell = this->GetRunPeriod().TargetCellFlag();

  for(int i = 0; i < _particles.Size(); i++){
    p4cor = Cpp_Momcor(_particles[i].P4(),_particles[i].Vertex(),targMat,
		       targCell,_runNumber);
    _particles[i].SetP4(p4cor);
  }
}
//_____________________________________________________________________________

double ClasEvent::_MainKFit(double __missingMass,bool __update,
			   const std::vector<bool> &__xC_meas,bool __xC_miss,
			   double __invariantMass){
  /// Private function for performing a kinematic fit.
  const int num_parts = _particles.Size();
  std::vector<TLorentzVector> p4(num_parts);
  std::vector<TVector3> vert(num_parts);
  TMatrixD covMatrix(_covMatTrack);

  for(int i = 0; i < num_parts; i++){
    p4[i] = _particles[i].P4();
    vert[i] = _particles[i].Vertex();
  }

  // get covariance matrix
  covMatrix = GetCovMatrix(_covMatTrack,p4,vert,_runNumber,_momentumCor,_isMC);

  // set the KinFit object for this event
  _kfit.SetEvent(_photon.E(),p4,covMatrix,this->TargetMass());  

  // run the fit
  _kfit.Fit(__missingMass,__xC_meas,__xC_miss,__invariantMass);

  if(__update){
    // update ClasEvent members
    this->SetChi2(_kfit.Chi2());
    this->SetNdf(_kfit.Ndf());
    _photon.SetPull(_kfit.GetPull(0));
    _photon.SetP4(_kfit.FitPhotonEnergy());
    float pulls[3];
    for(int i = 0; i < num_parts; i++){
      pulls[0] = _kfit.GetPull(3*i+1);
      pulls[1] = _kfit.GetPull(3*i+2);
      pulls[2] = _kfit.GetPull(3*i+3);
      _particles[i].SetPulls(pulls);
      _particles[i].SetP4(_kfit.FitP4(i));
    }
  }
  return _kfit.Prob();
}
//_____________________________________________________________________________

double ClasEvent::_RunBkgdKFit(const std::vector<TLorentzVector> &__p4,
			       const std::vector<TVector3> &__vert,int __flag,
			       double __missingMass,
			       const std::vector<bool> &__xC_meas,
			       bool __xC_miss,double __invariantMass) {
  /// Private function to run @a background kinematic fits.
  /** @param p4 Vector of 4-momenta of detected charged particles
   *  @param vert Vector of detected particle verticies
   *  @param flag Which KinFit fit function to use
   *  @param missingMass Mass of missing particle (negative means no particle)
   *  @param xC_meas Element i is whether i'th element is in extra constraint
   *  @param xC_miss Is missing particle in extra constraint
   *  @param invariantMass Mass for extra constraint (negative = no constraint)
   *
   *  The parameters <em>missingMass, xC_meas,xC_miss,invariantMass</em> are 
   *  passed directly to KinFit::Fit. The 4-momenta, @a p4, will be eloss 
   *  corrected....so they shouldn't be eloss corrected prior to being sent
   *  here. The covariance matrix is aquired using GetCovMatrix.
   *
   *  
   */
  const int num_part = (int)__p4.size();

  // perform eloss corrections
  TLorentzVector p4cor;
  int targMat = this->GetRunPeriod().TargetMaterialFlag();
  int targCell = this->GetRunPeriod().TargetCellFlag();

  std::vector<TLorentzVector> p4(__p4.size());
  for(int i = 0; i < num_part; i++){
    p4cor = Cpp_Momcor(__p4[i],__vert[i],targMat,targCell,_runNumber);
    p4[i] = p4cor;
  }

  TMatrixD covMatrix(_covMatTrack);
  // get covariance matrix
  covMatrix = GetCovMatrix(_covMatTrack,p4,__vert,_runNumber,_momentumCor,_isMC);
  _kfit.SetEvent(_photon.E(),p4,covMatrix,this->TargetMass());  

  // run the fit
  if(__flag == 0)
    _kfit.Fit(__missingMass,__xC_meas,__xC_miss,__invariantMass);
  
  else if(__flag == 1)
    _kfit.Fit2MissingTagged();
    
  return _kfit.Prob();
}
//_____________________________________________________________________________

double ClasEvent::BkgdKFit(const String &__names,double __missingMass) {
  /// Run a background kinematic fit.
  /** @param names Particle names string (same format as constructor 
   *  @param missingMass Mass of the missing particle
   *
   *  If @a missingMass isn't specified, then the functions assumes there is 
   *  no missing particle.
   *
   *  Background kinematic fits do @a NOT update any measured quantities.
   *
   *  <b>Important Note:</b> This function performs eloss corrections after
   *  changing any names to be changed as specified in @a names. Thus, eloss
   *  corrections should @a NOT be performed prior to calling this function.
   *
   */
  const int num_parts = _particles.Size();
  // set up 4-momentum vector and vertex vector
  std::vector<TLorentzVector> p4(num_parts);
  std::vector<TVector3> vert(num_parts);

  for(int i = 0; i < num_parts; i++) vert[i] = _particles[i].Vertex();
  this->_NamesToP4s(__names,p4); // get the 4-momenta
  
  return this->_RunBkgdKFit(p4,vert,0,__missingMass);
}
//_____________________________________________________________________________

double ClasEvent::BkgdKFit(const String &__names,const String &__miss) {
  /// Run a background kinematic fit.
  /** @param names Particle names string (same format as constructor)
   *  @param miss Missing particle name
   *
   *  <b>Returns:</b> Confidence level of the fit.
   *
   *  <b>Important Note:</b> If @a miss is <em>tagged photon</em>, then the 
   *  fit is performed with no missing particle, but instead the tagged photon
   *  is treated as missing. In this case, the measured photon energy is not
   *  used.
   *
   *
   *  <b>Important Note:</b> This function performs eloss corrections after
   *  changing any names to be changed as specified in @a names. Thus, eloss
   *  corrections should @a NOT be performed prior to calling this function.
   *
   */
  const int num_parts = _particles.Size();
  // set up 4-momentum vector and vertex vector
  std::vector<TLorentzVector> p4(num_parts);
  std::vector<TVector3> vert(num_parts);

  for(int i = 0; i < num_parts; i++) vert[i] = _particles[i].Vertex();
  this->_NamesToP4s(__names,p4); // get the 4-momenta
  
  if(__miss.find("tag") != String::npos && __miss.find("pho") != String::npos)
    return this->_RunBkgdKFit(p4,vert,1);
  else
    return this->_RunBkgdKFit(p4,vert,0,KinFit::NameToMass(__miss));
}    
//_____________________________________________________________________________

double ClasEvent::DiagnosticKFit(const String &__ommitPart,int __n){
  /// Run a diagnostic kinematic fit
  /** @param ommitPart Name of detected particle to ommit from the fit
   *  @param n Which @a ommitPart particle to ommit (defaults to 0)
   *
   *  A diagnostic kinematic fit performs takes an event with all particles
   *  detected (such as \f$ p,\pi^+,\pi^- \f$) and ignores one of them in a 
   *  1C kinematic fit. This is done to compare measured 4-momenta with 
   *  those obtained using the kinematics of the rest of the reaction.
   *
   *  Note: If @a ommitPart is <em>tagged photon </em, then the fit will omit
   *  the tagged photon.
   *
   */
  int num_ommitted = 1;
  if(__ommitPart.find("tag") != String::npos && 
     __ommitPart.find("phot") != String::npos){
    // fitting to missing tagged photon
    num_ommitted = 0;
  }
  const int num_parts = _particles.Size() - num_ommitted;

  std::vector<TLorentzVector> p4(num_parts);
  std::vector<TVector3> vert(num_parts);

  int ommitIndex = -1; 
  int ind = 0;
  if(num_ommitted > 0) ommitIndex = _particles.GetIndex(__ommitPart,__n);
  for(int i = 0; i < num_parts + num_ommitted; i++){
    if(i != ommitIndex){
      p4[ind] = _particles[i].P4();
      vert[ind] = _particles[i].Vertex();
      ind++;
    }
  }

  // build a temporary tracking matrix 
  TMatrixD covTrack(3*num_parts + 1,3*num_parts + 1);
  TMatrixD covMatrix;

  if(num_ommitted > 0){
    covTrack.Zero();
    covTrack(0,0) = _covMatTrack(0,0);

    int n_p = 0;
    for(int i = 0; i < num_parts + 1; i++){
      if(i != ommitIndex){
	for(int j = 1; j <= 3; j++){
	  for(int k = 1; k <= 3; k++){
	    covTrack(3*n_p + j,3*n_p + k) = _covMatTrack(3*i + j,3*i + k);
	  }
	}
	n_p++;
      }
    }
    covMatrix.ResizeTo(covTrack);
  }
  else
    covMatrix.ResizeTo(_covMatTrack);

  // get covariance matrix
  if(num_ommitted > 0)
    covMatrix = GetCovMatrix(covTrack,p4,vert,_runNumber,_momentumCor,_isMC);
  else
    covMatrix = GetCovMatrix(_covMatTrack,p4,vert,_runNumber,_momentumCor,_isMC);

  // set the KinFit object for this event
  _kfit.SetEvent(_photon.E(),p4,covMatrix,this->TargetMass());  

  // run the fit
  if(num_ommitted > 0){
    double miss_mass = _particles[ommitIndex].Mass();
    _kfit.Fit(miss_mass,std::vector<bool>(num_parts,false),false,-1.);
  }
  else
    _kfit.Fit2MissingTagged();

  // update ClasEvent members
  this->SetChi2(_kfit.Chi2());
  this->SetNdf(_kfit.Ndf());

  _photon.SetPull(_kfit.GetPull(0));
  _photon.SetP4(_kfit.FitPhotonEnergy());

  TLorentzVector p4_ommit = this->TotalP4();

  float pulls[3];
  ind = 0;
  for(int i = 0; i < num_parts + num_ommitted; i++){
    if(i != ommitIndex){
      pulls[0] = _kfit.GetPull(3*ind+1);
      pulls[1] = _kfit.GetPull(3*ind+2);
      pulls[2] = _kfit.GetPull(3*ind+3);
      _particles[i].SetPulls(pulls);
      _particles[i].SetP4(_kfit.FitP4(ind));
      
      p4_ommit -= _kfit.FitP4(ind);
      ind++;
    }
  }
  if(num_ommitted > 0){
    // now set the ommitted particle
    pulls[0] = 666;
    pulls[1] = 666;
    pulls[2] = 666;
    _particles[ommitIndex].SetPulls(pulls);
    _particles[ommitIndex].SetP4(p4_ommit);
  }

  return _kfit.Prob();
}  
//_____________________________________________________________________________

void ClasEvent::GetCalcMasses() {
  /// Recalculate the particle masses.
  /** Calculate the particle masses using the current photon's vertex time
   *  instead of the event start time defined by PART/EVNT. 
   *
   *  Sets each particle's time-of-flight (TOF) and calculated mass (CalcMass)
   *  along with the event vertex time (VertexTime) to be the photon's
   *  vertex time.
   */
  Particle particle;
  double phot_vtime = this->GetPhoton().VertexTime();
  double v_time = this->VertexTime();
  double p_mag,path,m_calc,tof;
  for(int p = 0; p < this->NumParticles(); p++){
    particle = _particles[p];
    tof = _particles[p].TOF() + v_time - phot_vtime;
    p_mag = _particles[p].P4().P();
    path = _particles[p].PathLength();
    
    m_calc = p_mag*sqrt(pow(__c_cm_per_ns__*tof/path,2) - 1.);

    _particles[p].SetTOF(tof);
    _particles[p].SetCalcMass(m_calc);
  }
  this->SetVertexTime(phot_vtime);
}
//_____________________________________________________________________________

