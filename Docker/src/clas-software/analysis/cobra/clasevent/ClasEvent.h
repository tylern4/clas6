// ClasEvent class header file. -*- C++ -*-
// Author: Mike Williams
/** @file clasevent/ClasEvent.h
 * @brief ClasEvent class definition file.
 */
#ifndef _CLASevent_H
#define _CLASevent_H
// System Headers:
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <cctype>
// ROOT Headers:
#include "TMatrixD.h"
// CLAS Headers:
#include "Eloss.h"
extern "C" {
#include "bosddl.h"
#include "vertex.h"
  // standard function in john mcnabb's vertex package
  int mtv_nobos(tber_t __tracks[],int __ntrk,vector3_t *__vert,float __cov[9], 
		float *__chi2,int *__iter);
  vector3_t get_1part_vert_nobos(tber_t __tracks[]);
  void vertex_doca(line_t *pline1, line_t *pline2, float *dp, float *pR);
}
// Local Headers:
#include "ClasRuns.h"
#include "Index.h"
#include "Event.h"
#include "KinFit.h"
#include "Constants.h"
// Conditional Headers:
#ifdef _compile_Pcor
#include "Pcor.h"
#endif
#ifdef _compile_g6cpcor
#include "g6cpcor.h"
#include "smear_mc_g6c.h"
#endif
#ifdef _compile_g1c2445pcor
#include "g1c2445pcor.h"
#include "smear_mc_g1c2445.h"
#endif
#ifdef _compile_g11pcor
#include "g11pcor.h"
#include "smear_mc.h"
#endif
#ifdef _compile_g10pcor
extern "C" {
#include "g10pcor.h"
}
#endif
//_____________________________________________________________________________

typedef std::string String; 

//_____________________________________________________________________________

class ClasEvent : public Event {

private:
  // Data Members (private):
  Index _index; ///< Keeps track of all particle/photon indicies
  TMatrixD _covMatTrack; ///< Tracking covariance matrix
  KinFit _kfit; ///< Stores all info on last kinematic fit run
  bool _momentumCor; ///< Have momentum corrections been applied?
  bool _isMC; ///< Is this monte carlo?
  ParticleArray<Particle> _ignored_particles; ///< Detected particles ignored
  
  // Functions (private):
  
  void _Copy(const ClasEvent &__ce){
    /// Copy @a ce's data members (just ClasEvent members @a NOT Event members)
    _index = __ce._index;
    _covMatTrack = __ce._covMatTrack;
    _kfit = __ce._kfit;
    _momentumCor = __ce._momentumCor;
    _isMC = __ce._isMC;
  }
  
  void _SetTargetP4(double __mass){
    /// Set target 4-momentum given its mass.
    if(__mass != this->TargetMass()) this->Event::SetTargetP4(__mass);
  }
  
  template<typename _InterTp> void _GetCovMatTrack(_InterTp __inter) {
    /// Sets up the tracking covariance matrix (_InterTp is the interface type)
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("ClasEvent::_GetCovMatTrack");
    abort();
  }
  
  inline void _NamesToP4s(const String &__names,
			  std::vector<TLorentzVector> &__p4) const {
    /// Private function to fill @a p4 with particles' p3 with ids @a names
    __p4.resize(_particles.Size());
    for(int i = 0; i < _particles.Size(); i++) __p4[i] = _particles[i].P4();
    if(__names == String()) return;
    
    ParticleArray<int> array(__names);
    // check size is correct
    if(array.Size() != _particles.Size()){
      std::cout << "Warning! <ClasEvent> Partcle names string " << __names 
		<< " has " << array.Size() << " particles but ClasEvent has "
		<< _particles.Size() << std::endl;
      abort();
    }
    for(int i = 0; i < array.Size(); i++)
      __p4[i].SetE(sqrt(array[i].M()*array[i].M() + __p4[i].P()*__p4[i].P()));
  }
  
  // private kinematic fitting function for "background" fits
  double _RunBkgdKFit(const std::vector<TLorentzVector> &__p4,
		      const std::vector<TVector3> &__vert,
		      int __flag,double __missingMass = -1.,
		      const std::vector<bool> &__xC_meas = std::vector<bool>(),
		      bool __xC_miss = false,double __invariantMass = -1.);
  
  // private kinematic fitting function for "normal" fits
  double _MainKFit(double __missingMass = -1.,bool __update = true,
		   const std::vector<bool> &__xC_meas = std::vector<bool>(),
		   bool __xC_miss = false,double __invariantMass = -1.);
  
public:
  
  // Create/Copy/Destroy:
  
  // build event with particles given in p_names
  ClasEvent(const String &__p_names);
  
  ClasEvent(const ClasEvent &__ce):Event(__ce){
    /// Copy Constructor
    this->_Copy(__ce);
  }
  
  virtual ~ClasEvent(){
    /// Destructor
  }
  
  // assignment operator
  ClasEvent& operator=(const ClasEvent &__ce);
  
  ClasEvent& operator=(const Event &__ev){
    /// Set the Event elements to be same as @a ev.
    this->Event::operator=(__ev);
    return *this;
  }
  
  // Getters:
  
  inline const ClasRunPeriod& GetRunPeriod() const {
    /// Returns the ClasRunPeriod object for the current run number  
    return ClasRuns::Instance()->GetRunPeriod(this->RunNumber());
  }

  inline const TMatrixD& GetCovMatTrack() const {
    /// Returns the tracking covariance matrix     
    return _covMatTrack;
  }
  
  inline const TMatrixD& GetCovMatFull() const {
    /// Returns the full covariance matrix of the last kinematic fit run
    return _kfit.GetCovMat();
  }
  
  inline const KinFit& GetKinFit() const {
    /// Returns the KinFit object with all info from last kinematic fit run
    return _kfit;
  }
  
  inline bool IsMC() const {
    /// Returns if the current event is a monte carlo event
    return _isMC;
  }
  
  inline void GetMomenta(ParticleArray<TLorentzVector> &__p4array) const {
    /// Fills @a p4array with the detected 4-momenta
    String names = _particles.GetNamesString();
    __p4array = ParticleArray<TLorentzVector>::ParticleArray(names);
    for(int p = 0; p < this->NumParticles(); p++) 
      __p4array[p] = _particles[p].P4();   
  }
  
  inline int GetParticleDataIndex(const String &__p,int __n = 0) const {
    /** Returns the index of the <em>(p,n)</em> particle in the CLAS data
     *	interface being used. This could be the index to the Charged branch
     *  in the CLASdata interface, the index to the PART bank or EVNT bank for
     *  the BOS interface (depending on the particle id scheme used), etc...
     */
    int array_index = this->GetParticleIndex(__p,__n);
    if(array_index < 0) return array_index;
    return _index.GetIndicies()[array_index];
  }
  
  // Index class wrappers:
  
  inline const String& IdScheme() const {
    /// Returns the particle id scheme (ex. @a seb)     
    return _index.IdScheme();
  } 
  
  inline int NumCharged() const {
    /// Returns the number of charged particles detected in the event.     
    return _index.NumCharged();
  }
  
  inline int NumPhotons() const {
    /// Returns the number of @a good photons detected in the event.     
    return _index.NumPhotons();
  }

  inline int NumEventCombos() const {
    /// Returns the number of event combos that can be made in this event.    
    return _index.NumEventCombos();
  }
  
  inline int GetPhotonIndex() const {
    /// Returns the current photon index to the interface file
    return _index.GetPhotonIndex();
  }
  
  inline const ParticleArray<Particle>& IgnoredParticles() const {
    /// Returns detected charged particles ignored by current event combo
    return _ignored_particles;
  }

  // Setters:
  
  inline void SetRunNumber(int __nrun) {
    /// Set run number and, if needed, the target 4-momentum
    if(__nrun != _runNumber){
      _runNumber = __nrun;
      this->_SetTargetP4(ClasRuns::Instance()->GetRunPeriod(__nrun).TargetMass());
    }
  }
  
  inline void SetPhotonEnergy(float __erg) {
    /// Set the tagged photon energy     
    _photon.SetP4(__erg);
  }
  
  inline void SetParticleP4(const TLorentzVector &__p4,
			    const String &__name,int __n = 0){
    /// Set <em>(name,n)</em> particle's 4-momentum to be @a p4.
    this->_particles(__name,__n).SetP4(__p4);
  }
  
  inline void SetParticleVertex(const TVector3 &__v,const String &__name,
				int __n = 0){
    /// Set <em>(name,n)</em> particle's vertex to @a v
    this->_particles(__name,__n).SetVertex(__v);
  }

  inline void SetIsMC(bool __is_mc){
    /// Set whether or not this is monte carlo
    _isMC = __is_mc;
  }
  
  inline void SetTimingCut(float __dt){
    /// Set photon selection timing cut to @a dt (see Index for details)
    _index.SetTimingCut(__dt);
  }
  
  inline void SetNoTimingCut(){
    /// Turn off the photon selection timing cut
    _index.SetTimingCut(1.e9);
  }
  
  inline void SetMomentumCor(bool __hasBeenCorrected = true) {
    /// Have momentum corrections been applied?
    _momentumCor = __hasBeenCorrected;
  }
  
  // Functions:
  
  void UseSEB(float __dt = 1.0){
    /// Use the @a seb particle id scheme.
    /** @a dt is cut to be made on the @a seb variable dt */
    _index.SetTimingCut(__dt);
    _index.SetIdScheme("seb");
  }

  void ResetParticleNames(){
    /// Reset particle names to what Index is looking for (orginal names)
    for(int i = 0; i < _particles.Size(); i++)
      _particles[i].SetName(_index.GetParticles()[i].Name());
  }
  
  void ResetParticles(){
    /// Reset particle names and masses to orginal values
    for(int i = 0; i < _particles.Size(); i++){
      _particles[i].SetName(_index.GetParticles()[i].Name());
      _particles[i].SetMass(_index.GetParticles()[i].Mass());
    }
  }
  
  void SetParticles(const String &__p_names){
    /// Change the particle names
    /** @param p_names Particle names string of same format as in the @a ctor
     *
     *  The order of the names in @a p_names should be the same as the order
     *  the particles are stored in (ie. same order as they were set in the 
     *  @a ctor). This function not only changes the link between the particles
     *  and the elements in the ParticleTypes global particle array, but also
     *  uses the new mass to change the energy component of the 4-momentum.
     *
     *  Note: This does not change the particle names in the Index class...ie,
     *  this doesn't change what ClasEvent is looking for, only the id of the
     *  particles currently stored in the current event combination.
     *
     *  Note: The particle names are automatically reset by ClasEvent::GetEvent
     *
     */
    ParticleArray<int> array(__p_names);
    
    if(array.Size() != _particles.Size()){
      std::cout << "Warning! <ClasEvent::SetParticles> Size mismatch, "
		<< "ClasEvent has " << _particles.Size() << " particles but "
		<< "only " << array.Size() << " names were passed in. Names "
		<< "will NOT be changed." << std::endl;
      return;
    }
    for(int i = 0; i < array.Size(); i++){
      _particles[i].SetName(array[i].Name());
      _particles[i].SetMass(array[i].Mass());
    }
  }
  
  // do eloss corrections on all detected charged particles
  void MomCor();
  
  void ElossCor(){
    /// Apply energy loss corrections to all detected charged particles.
    this->MomCor();
  }
  
  // Kinematic fitting functions:
  double KinematicFit(const String &__miss = String()){
    /// Kinematically fit the data to @a miss missing particle.
    /** If @a miss isn't specified (KinematicFit()), then the fit is run to
     *  nothing missing.
     *
     *  <b>Returns: </b> Confidence level of the fit.
     */
    return this->_MainKFit(KinFit::NameToMass(__miss),true);
  }
  
  double KinematicFit(double __missingMass){
    /// Kinematically fit to a missing particle with mass @a missingMass
    /** <b>Returns: </b> Confidence level of the fit. */
    return this->_MainKFit(__missingMass,true);
  }
  
  double KinematicFit(const String &__miss,const String &__pnames,
		      double __inv_mass){
    /** @param miss Name of the missing particle 
     *  @param pnames String specifying particles in extra mass constraint
     *  @param inv_mass Mass to contrain @a pnames to
     * 
     *  Perform a kinematic fit with an extra mass constraint. The string 
     *  @a pnames should give all particle names you want to include in the
     *  extra mass constraint in a :-delimated list (ex. "pi+:pi-"). If the 
     *  missing particle is to be included in the extra constraint, then put
     *  @a missing into @a pnames. For example, to fit to a missing pi0 and to
     *  also constrain the 3pi mass to be an omega (assuming we have a 
     *  ClasEvent created with clasevent("p:pi+:pi-")), you could do 
     *  clasevent.KinematicFit("pi0","pi+:pi-:missing",0.78257)
     */
    std::vector<bool> constrain(this->NumParticles());
    ParticleArray<int> array(__pnames); 
    for(int p = 0; p < this->NumParticles(); p++)
      constrain[p] = array.HasParticle(this->_particles[p].Name());
    bool missing_in_constraint = (__pnames.find("missing") != String::npos);

    return this->_MainKFit(KinFit::NameToMass(__miss),true,constrain,
			   missing_in_constraint,__inv_mass);
  }

  // run a background kinematic fit
  double BkgdKFit(const String &__names,double __missingMass = -1.);

  double BkgdKFit(const String &__names,const String &__miss);

  // run a diagnostic kinematic fit
  double DiagnosticKFit(const String &__ommitPart,int __n = 0);

  // print out the ClasEvent configuration
  void PrintConfig() const;

  // recalculate the particle masses using the current photon's vertex time
  void GetCalcMasses();

  // Interface template functions:

  template<typename _InterTp> void SetClasEvent(_InterTp __inter,
						int __runNumber){
    /// Intialize the ClasEvent object for a new event
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("ClasEvent::SetClasEvent");
    abort();
  }

  template<typename _InterTp> void SetClasEvent(_InterTp __inter){
    /// Intialize the ClasEvent object for a new event
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("ClasEvent::SetClasEvent");
    abort();
  }

  template<typename _InterTp> bool GetEvent(_InterTp __inter){
    /// Sets up everything for next particle/photon combo in the event    
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("ClasEvent::GetEvent");
    abort();
    return false; // for some reason some compilers want this here
  }

  // Momentum Corrections (compiled in if configured to):

#ifdef _compile_Pcor
  // g1c/g11 run period momentum corrections

  inline void CorrectTagger_Pcor(int __runId){
    /// Correct the tagged photon energy.
    /** @param runId Run period id number to pass to TAGRcor function
     *
     *  This function uses a tagger correction from packages/utilities/Pcor.
     *  @a _compile_Pcor must be defined for this function to be compiled into
     *  ClasEvent.
     *
     */
    double e0 = this->GetRunPeriod().BeamEnergy();
    _photon.SetP4(TAGRcor(_photon.E(),e0,__runId));
    _momentumCor = true; // this is enough of a correction to set this
  }

  inline void CorrectMomenta_Pcor(int __runId){
    /// Perform momentum corrections on all detected charged particles.
    /** @param runId Run period id number to pass to Pcor function
     *
     *  This function uses momentum corrections from packages/utilities/Pcor.
     *  @a _compile_Pcor must be defined for this function to be compiled into
     *  ClasEvent.
     *
     *  Note: This function runs InitPcor the first time it's called.
     */
    static bool run_init = true;
    if(run_init){
      InitPcor();
      run_init = false;
    }
    vector4_t p4cor,p4meas; // CLAS 4-vector type
    TLorentzVector p4; // ROOT 4-vector type
    for(int i = 0; i < _particles.Size(); i++){
      p4meas.space.x = _particles[i].Px();
      p4meas.space.y = _particles[i].Py();
      p4meas.space.z = _particles[i].Pz();
      p4meas.t = _particles[i].E();
      p4cor = Pcor(p4meas,_particles[i].Sector(),_particles[i].Q(),__runId);
      p4.SetPxPyPzE(p4cor.space.x,p4cor.space.y,p4cor.space.z,p4cor.t);
      _particles[i].SetP4(p4);
    }
    _momentumCor = true;
  }
#endif /* _compile_Pcor */


#ifdef _compile_g1c2445pcor
  // g1c2445 momentum/tagger corrections from g1c2445pcor directory

  inline void CorrectTagger_g1c2445pcor(){
    /// Correct the tagged photon energy.
    /** 
     *  This function uses a tagger correction from packages/utilities/g1c2445pcor.
     *  @a _compile_g1c2445pccor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    int eid = _photon.E_Id();
    float e_beam = this->GetRunPeriod().BeamEnergy();
    _photon.SetP4(g1c2445TaggerCor(_photon.E(),e_beam,eid,this->RunNumber()));
    _momentumCor = true; // this is enough of a correction to set this
  }

  inline void CorrectMomenta_g1c2445pcor(){
    /// Perform momentum corrections on all detected charged particles.
    /** 
     *  This function uses momentum corrections from packages/utilities/g1c2445pcor
     *  @a _compile_g1c2445pcor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    float p3m[3],p3c[3];
    TLorentzVector p4; // ROOT 4-vector type
    for(int i = 0; i < _particles.Size(); i++){
      p3m[0] = _particles[i].Px();
      p3m[1] = _particles[i].Py();
      p3m[2] = _particles[i].Pz();
      g1c2445MomCor(p3m,_particles[i].Q(),p3c);
      double p = sqrt(p3c[0]*p3c[0] + p3c[1]*p3c[1] + p3c[2]*p3c[2]);
      double e = sqrt(p*p + pow(_particles[i].Mass(),2));
      p4.SetPxPyPzE(p3c[0],p3c[1],p3c[2],e);
      _particles[i].SetP4(p4);
    }
    _momentumCor = true;
  }


#endif /* _compile_g1c2445pcor */

#ifdef _compile_g6cpcor
  // g6c momentum/tagger corrections from g6cpcor directory

  inline void CorrectTagger_g6cpcor(){
    /// Correct the tagged photon energy.
    /** 
     *  This function uses a tagger correction from packages/utilities/g6cpcor.
     *  @a _compile_g6cpccor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    int eid = _photon.E_Id();
    float e_beam = this->GetRunPeriod().BeamEnergy();
    _photon.SetP4(g6cTaggerCor(_photon.E(),e_beam,eid,this->RunNumber()));
    _momentumCor = true; // this is enough of a correction to set this
  }

  inline void CorrectMomenta_g6cpcor(){
    /// Perform momentum corrections on all detected charged particles.
    /** 
     *  This function uses momentum corrections from packages/utilities/g6cpcor
     *  @a _compile_g6cpcor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    float p3m[3],p3c[3];
    TLorentzVector p4; // ROOT 4-vector type
    for(int i = 0; i < _particles.Size(); i++){
      p3m[0] = _particles[i].Px();
      p3m[1] = _particles[i].Py();
      p3m[2] = _particles[i].Pz();
      g6cMomCor(p3m,_particles[i].Q(),p3c);
      double p = sqrt(p3c[0]*p3c[0] + p3c[1]*p3c[1] + p3c[2]*p3c[2]);
      double e = sqrt(p*p + pow(_particles[i].Mass(),2));
      p4.SetPxPyPzE(p3c[0],p3c[1],p3c[2],e);
      _particles[i].SetP4(p4);
    }
    _momentumCor = true;
  }


#endif /* _compile_g6cpcor */

#ifdef _compile_g11pcor
  // g11 momentum/tagger corrections from g11pcor directory

  inline void CorrectTagger_g11pcor(){
    /// Correct the tagged photon energy.
    /** 
     *  This function uses a tagger correction from packages/utilities/g11pcor.
     *  @a _compile_g11pccor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    int eid = _photon.E_Id();
    float e_beam = this->GetRunPeriod().BeamEnergy();
    _photon.SetP4(g11TaggerCor(_photon.E(),e_beam,eid,this->RunNumber()));
    _momentumCor = true; // this is enough of a correction to set this
  }

  inline void CorrectMomenta_g11pcor(){
    /// Perform momentum corrections on all detected charged particles.
    /** 
     *  This function uses momentum corrections from packages/utilities/g11pcor
     *  @a _compile_g11pcor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    float p3m[3],p3c[3];
    TLorentzVector p4; // ROOT 4-vector type
    for(int i = 0; i < _particles.Size(); i++){
      p3m[0] = _particles[i].Px();
      p3m[1] = _particles[i].Py();
      p3m[2] = _particles[i].Pz();
      g11MomCor(p3m,_particles[i].Q(),p3c);
      double p = sqrt(p3c[0]*p3c[0] + p3c[1]*p3c[1] + p3c[2]*p3c[2]);
      double e = sqrt(p*p + pow(_particles[i].Mass(),2));
      p4.SetPxPyPzE(p3c[0],p3c[1],p3c[2],e);
      _particles[i].SetP4(p4);
    }
    _momentumCor = true;
  }

  //inline void SmearMomenta() {
  /// Smear 3-momentum for each detected charged particle
  /**
   *  This function uses a smearing function  from packages/utilities/g11pcor
   *  @a _compile_g11pcor must be defined for this function to be compiled 
   *  into ClasEvent (see $COBRASYS/configure --help for details).
   */
  /*
    int run_num = this->RunNumber();
    bool did_smear = false;
    float p3m[3],p3s[3];
    TLorentzVector p4; // ROOT 4-vector type
    for(int i = 0; i < _particles.Size(); i++){
    p3m[0] = _particles[i].Px();
    p3m[1] = _particles[i].Py();
    p3m[2] = _particles[i].Pz();
    #ifdef _compile_g1c2445pcor 
    if(!did_smear){
    if(run_num >= 21763 && run_num <= 21983){
    SmearMomentum_g1c2445(p3m,_particles[i].CalcMass(),
    sqrt(_covMatTrack(3*i+1,3*i+1)),
    sqrt(_covMatTrack(3*i+2,3*i+2)),
    sqrt(_covMatTrack(3*i+3,3*i+3)),_particles[i].Q(),p3s);
    did_smear = true;
    }
    }
    #endif
    #ifdef _compile_g11pcor 
    if(!did_smear){
    if(run_num >= 43490 && run_num <= 44133){
    SmearMomentum(p3m,_particles[i].CalcMass(),
    sqrt(_covMatTrack(3*i+1,3*i+1)),
    sqrt(_covMatTrack(3*i+2,3*i+2)),
    sqrt(_covMatTrack(3*i+3,3*i+3)),_particles[i].Q(),p3s);
    did_smear = true;
    }
    }
    #endif
    if(!did_smear){
    std::cout << "<ClasEvent::SmearMomenta> Warning! No correction found"
    << " for run: " << run_num << std::endl;
    }

    double p = sqrt(p3s[0]*p3s[0] + p3s[1]*p3s[1] + p3s[2]*p3s[2]);
    double e = sqrt(p*p + pow(_particles[i].Mass(),2));
    p4.SetPxPyPzE(p3s[0],p3s[1],p3s[2],e);
    _particles[i].SetP4(p4);
    }
    }
  */

#endif /* _compile_g11pcor */

#ifdef _compile_g10pcor

  // g10 run period momentum corrections...not implemented yet
  void CorrectMomenta_g10pcor(int __icor){
    /// Perform momentum corrections on all detected charged particles.
    /** 
     * This function uses momentum corrections from packages/utilities/g10pcor
     * @a _compile_g10pcor must be defined for this function to be compiled 
     * into ClasEvent (see $COBRASYS/configure --help for details).
     *
     */
    vector4_t p4;
    TLorentzVector p4_set; // ROOT 4-vector type
    int flag;
    int torus = (int)(this->GetRunPeriod().TorusCurrent());
    for(int i = 0; i < _particles.Size(); i++){
      p4.space.x = _particles[i].Px();
      p4.space.y = _particles[i].Py();
      p4.space.z = _particles[i].Pz();
      p4.t = _particles[i].E();
      flag = c_g10pcor(torus,_particles[i].PDG_Id(),__icor,&p4);
      p4_set.SetPxPyPzE(p4.space.x,p4.space.y,p4.space.z,p4.t);
      _particles[i].SetP4(p4_set);
    }
    _momentumCor = true;
  }

#endif /* _compile_g10pcor */

  inline void CorrectTagger(int __flag = -1) {
    /// Corrects the tagged photon energy
    int run_num = this->RunNumber();
    bool did_correct = false;
#ifdef _compile_Pcor
    if(__flag != -1) {
      this->CorrectTagger_Pcor(__flag);
      did_correct = true;
    }
    else if(run_num >= 20926 && run_num <= 21359){
      this->CorrectTagger_Pcor(2);
      did_correct = true;
    }
    else if(run_num >= 21763 && run_num <= 21983){
      this->CorrectTagger_Pcor(1);
      did_correct = true;
    }
#endif 
#ifdef _compile_g6cpcor 
    if(!did_correct){
      if(run_num >= 29808 && run_num <= 30198){
	this->CorrectTagger_g6cpcor();
	did_correct = true;
      }
    }
#endif
#ifdef _compile_g1c2445pcor 
    if(!did_correct){
      if(run_num >= 21763 && run_num <= 21983){
	this->CorrectTagger_g1c2445pcor();
	did_correct = true;
      }
    }
#endif
#ifdef _compile_g11pcor 
    if(!did_correct){
      if(run_num >= 43490 && run_num <= 44133){
	this->CorrectTagger_g11pcor();
	did_correct = true;
      }
    }
#endif
    if(!did_correct){
      std::cout << "<ClasEvent::CorrectTagger> Warning! No tagger correction "
		<< "found for run: " << run_num << std::endl;
    }
    _momentumCor = true; // this is enough of a correction to set this
  }

  inline void CorrectMomenta(int __flag = -1) {
    /// Corrects the charged particle 3-momenta
    int run_num = this->RunNumber();
    bool did_correct = false;
#ifdef _compile_Pcor
    if(__flag != -1) {
      this->CorrectMomenta_Pcor(__flag);
      did_correct = true;
    }
    else if(run_num >= 20926 && run_num <= 21359){
      this->CorrectMomenta_Pcor(2);
      did_correct = true;
    }
    else if(run_num >= 21763 && run_num <= 21983){
      this->CorrectMomenta_Pcor(1);
      did_correct = true;
    }
#endif 
#ifdef _compile_g6cpcor 
    if(!did_correct){
      if(run_num >= 29808 && run_num <= 30198){
	this->CorrectMomenta_g6cpcor();
	did_correct = true;
      }
    }
#endif
#ifdef _compile_g1c2445pcor 
    if(!did_correct){
      if(run_num >= 21763 && run_num <= 21983){
	this->CorrectMomenta_g1c2445pcor();
	did_correct = true;
      }
    }
#endif
#ifdef _compile_g11pcor 
    if(!did_correct){
      if(run_num >= 43490 && run_num <= 44133){
	this->CorrectMomenta_g11pcor();
	did_correct = true;
      }
    }
#endif
#ifdef _compile_g10pcor 
    if(!did_correct){
      if(run_num >= 42299 && run_num <= 43228){
	this->CorrectMomenta_g10pcor(__flag);
	did_correct = true;
      }
    }
#endif
    if(!did_correct){
      std::cout << "<ClasEvent::CorrectMomenta> Warning! No correction found"
		<< " for run: " << run_num << std::endl;
    }
    _momentumCor = true; // this is enough of a correction to set this
  } 


  inline void SmearMomenta() {
    /// Smear 3-momentum for each detected charged particle
    /**
     *  This function uses a smearing function  from packages/utilities/g11pcor
     *  @a _compile_g11pcor or @a _compile_g1cpcor must be defined for this function to be compiled 
     *  into ClasEvent (see $COBRASYS/configure --help for details).
     */
    int run_num = this->RunNumber();
    float p3m[3],p3s[3];
    TLorentzVector p4; // ROOT 4-vector type
    for(int i = 0; i < _particles.Size(); i++){
      bool did_smear = false;
      p3m[0] = _particles[i].Px();
      p3m[1] = _particles[i].Py();
      p3m[2] = _particles[i].Pz();
#ifdef _compile_g6cpcor 
      if(!did_smear){
	if(run_num >= 29808 && run_num <= 30198){
	  SmearMomentum_g6c(p3m,_particles[i].CalcMass(),
			    sqrt(_covMatTrack(3*i+1,3*i+1)),
			    sqrt(_covMatTrack(3*i+2,3*i+2)),
			    sqrt(_covMatTrack(3*i+3,3*i+3)),_particles[i].Q(),p3s);
	  did_smear = true;
	}
      }
#endif
#ifdef _compile_g1c2445pcor 
      if(!did_smear){
	if(run_num >= 21763 && run_num <= 21983){
	  SmearMomentum_g1c2445(p3m,_particles[i].CalcMass(),
				sqrt(_covMatTrack(3*i+1,3*i+1)),
				sqrt(_covMatTrack(3*i+2,3*i+2)),
				sqrt(_covMatTrack(3*i+3,3*i+3)),_particles[i].Q(),p3s);
	  did_smear = true;
	}
      }
#endif
#ifdef _compile_g11pcor 
      if(!did_smear){
	if(run_num >= 43490 && run_num <= 44133){
	  SmearMomentum(p3m,_particles[i].CalcMass(),
			sqrt(_covMatTrack(3*i+1,3*i+1)),
			sqrt(_covMatTrack(3*i+2,3*i+2)),
			sqrt(_covMatTrack(3*i+3,3*i+3)),_particles[i].Q(),p3s);
	  did_smear = true;
	}
      }
#endif
      if(!did_smear){
	std::cout << "<ClasEvent::SmearMomenta> Warning! No correction found"
		  << " for run: " << run_num << std::endl;
      }

      double p = sqrt(p3s[0]*p3s[0] + p3s[1]*p3s[1] + p3s[2]*p3s[2]);
      double e = sqrt(p*p + pow(_particles[i].Mass(),2));
      p4.SetPxPyPzE(p3s[0],p3s[1],p3s[2],e);
      _particles[i].SetP4(p4);
    }
  }

  /** @param inter Interface type
   *  @param inds  Indicies of particles to use
   */
  template<typename _InterTp> 
  TVector3 GetVertex(_InterTp __inter,const std::vector<int> &__inds){    
    PrintDummyTemplateMessage("ClasEvent::GetVertex");
    abort();
    return TVector3(); 
  }

  template<typename _InterTp> 
  float GetDOCA(_InterTp __inter,const std::vector<int> &__inds){    
    PrintDummyTemplateMessage("ClasEvent::GetDOCA");
    abort();
    return 999.0;
  }

};

//_____________________________________________________________________________

#endif /* _ClasEvent_H */
