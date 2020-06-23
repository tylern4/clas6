// Event class header file. -*- C++ -*-
// Author: Mike Williams 3/20/2005
/** @file event/Event.h
 * @brief Event class definition file.
 */
//_____________________________________________________________________________
#ifndef _Event_H
#define _Event_H
// System Headers:
#include <iostream>
// ROOT Headers:
#include "TObject.h"
#include "TLorentzVector.h"
#include "TVector3.h"
// Local Headers:
#include "Particle.h"
#include "Photon.h"
#include "ParticleArray.h"
//_____________________________________________________________________________

// Global Types:
typedef std::string String; 

//_____________________________________________________________________________

class Event : public TObject {

protected:
  // Data Members (protected):
  ParticleArray<Particle> _particles; ///< Charged particle info
  Photon _photon; ///< Photon info
  TLorentzVector _p4target; ///< Target 4-momentum
  TVector3 _vertex; ///< Interaction vertex
  Int_t _runNumber; ///< CLAS run number
  Int_t _eventNumber; ///< The event number in this run
  Float_t _vertexTime; ///< The event's vertex time (or start time)
  Float_t _chi2; ///< \f$\chi^2\f$ from kinematic fitting
  Int_t _ndf; ///< Number of degrees of freedom from kinematic fitting

  // Functions (private):
  // copy Event data members
  void _Copy(const Event &__ev);

public:
  // Create/Copy/Destroy:

  Event():TObject(){
    /// Default Constructor
    _runNumber = 0;
  }

  Event(const String &__p_names):TObject(),_particles(__p_names){
    /// Constructor
    /** @param p_names String of format <em>"a:b:c:..."</em>
     *
     *  Constructs an event with particles <em>a,b,c,...</em>. See 
     *  ParticleArray for string parsing info.
     */
    _runNumber = 0;
  }

  Event(const Event &__ev):TObject(__ev){
    /// Copy Constructor
    this->_Copy(__ev);
  }

  virtual ~Event(){
    /// Destructor
  }

  // assignment operator
  Event& operator=(const Event &__ev);
  
  // Functions:

  // Setters:

  inline bool AddParticle(const String &__name,const Particle &__p){
    /// Add a particle to the event. 
    /** Returns @a true if the particle was able to be added, @a false 
     *  otherwise. See ParticleArray::Add for details.
     */
    return _particles.Add(__name,__p);
  }

  inline void RemoveParticle(const String &__name,int __n = 0){
    /// Remove particle <em>(name,n)</em> from the event
    _particles.Remove(__name,__n);
  }

  inline void SetParticle(const Particle &__p,const String &__name,int __n =0){
    /// Set charged particle info for <em>(name,n)</em> particle.
    _particles(__name,__n) = __p;
  }

  inline void SetPhoton(const Photon &__phot) {
    /// Set tagged photon info     
    _photon = __phot;
  }

  inline void SetPhotonEnergy(double __erg){
    /// Sets the tagged photon energy to @a erg
    _photon.SetP4(__erg);
  }

  inline void SetRunNumber(int __nrun) {
    /// Set CLAS run number 
    _runNumber = __nrun;
  }

  inline void SetEventNumber(int __nevent) {
    /// Set event number in this CLAS run number     
    _eventNumber = __nevent;
  }

  inline void SetVertex(const TVector3 &__vert) {
    /// Set interaction vertex     
    _vertex = __vert;
  }

  inline void SetChi2(float __chi2) {
    /// Set \f$ \chi^2 \f$ from kinematic fitting     
    _chi2 = __chi2;
  }

  inline void SetNdf(int __ndf) {
    /// Set number of degrees-of-freedom from kinematic fitting     
    _ndf = __ndf;
  }

  inline void SetTargetP4(const TLorentzVector &__p4targ){
    /// Set target 4-momentum     
    _p4target =__p4targ;
  }

  inline void SetTargetP4(double __mass){
    /// Set target 4-momentum given its mass     
    _p4target.SetXYZT(0.,0.,0.,__mass);
  }

  inline void SetVertexTime(float __vtime) {
    /// Set interaction vertex time     
    _vertexTime = __vtime;
  }

  // Getters:

  inline const Particle& GetParticle(const String &__p,int __n = 0) const {
    /// Returns a constant reference to the <em>(p,n)</em> particle.
    /** @param p Particle name
     *  @param n Which @a p particle to get (defaults to 0)
     *
     *  Note: @a n is a @a C-style index, so @a n = 0 is the 1st @a p, @a n = 1
     *  is the 2nd @a p,...
     *
     *  <b> Example Usage: </b>
     *
     *  \include Event_GetParticle.ex
     */
    return _particles(__p,__n);
  }

  inline int GetParticleIndex(const String &__p,int __n = 0) const {
    /// Returns the array index of the <em>(p,n)</em> particle.
    return _particles.GetIndex(__p,__n);
  }

  inline const Particle& GetParticle(int __i) const {
    /// Returns a constant reference to the @a i'th Particle object.
    return _particles[__i];
  }
  
  inline const PArrayElement<Particle>& GetElement(const String &__p,
						   int __n = 0) const {
    /// Returns a constant reference to the <em>(p,n)</em> PArrayElement.
    /** @param p Particle name
     *  @param n Which @a p particle to get (defaults to 0)
     *
     *  Note: @a n is a @a C-style index, so @a n = 0 is the 1st @a p, @a n = 1
     *  is the 2nd @a p,...
     *
     *  Same as GetParticle(const String&,int) except it returns the full
     *  PArrayElement object instead of just the Particle
     */
    return _particles(__p,__n);
  }

  inline const PArrayElement<Particle>& GetElement(int __i) const {
    /// Returns a constant reference to the @a i'th PArrayElement object.
    return _particles[__i];
  }

  inline bool HasParticle(const String &__p,int __n = 0) const {
    /// Is Event storing the <em>(p,n)</em> particle?
    int index = _particles.GetIndex(__p,__n);
    if(index < 0) return false;
    else return true;
  }

  inline const TVector3& Vertex() const {
    /// Returns the interaction vertex 
    return _vertex;
  }

  inline Int_t RunNumber() const {
    /// Returns the CLAS run number     
    return _runNumber;
  }

  inline Int_t EventNumber() const {
    /// Returns the event number in this event's CLAS run number     
    return _eventNumber;
  }

  inline const Photon& GetPhoton() const {
    /// Returns the Photon object storing @a ALL photon info     
    return _photon;
  }

  inline const TLorentzVector& P4Target() const {
    /// Returns the target 4-momentum     
    return _p4target;
  }

  inline const TLorentzVector& TargetP4() const {
    /// Returns the target 4-momentum (same as P4Target())     
    return _p4target;
  }

  inline Int_t Ndf() const {
    /// Returns the number of degrees-of-freedom from kinematic fitting     
    return _ndf;
  }

  inline Float_t Chi2() const {
    /// Returns \f$ \chi^2 \f$ from kinematic fitting     
    return _chi2;
  }

  inline Float_t VertexTime() const {
    /// Returns the interaction vertex time     
    return _vertexTime;
  }

  inline Int_t NumParticles() const {
    /// Returns the number of particles currently being stored     
    return _particles.Size();
  }
  
  // Functions:

  inline float Prob() const {
    /// Returns the confidence level from kinematic fitting     
    return TMath::Prob(_chi2,_ndf);
  }

  inline float ConfidenceLevel() const {
    /// Returns the confidence level (same as Prob())     
    return this->Prob();
  }
  
  inline double TargetMass() const {
    /// Returns the mass of the target     
    return _p4target.M();
  }

  // zero's the data members
  void Zero();

  void Clear(){
    /// Clears the ParticleArray, zeroes everything else.
    _particles.Clear();
    this->Zero();
  }

  // prints the event info to os
  void Print(std::ostream &__os = std::cout) const;

  // executes a vertex timing cut
  Bool_t TimingCut(float __t) const;

  // returns the name of the target (lh2 for liquid hydrogen, etc)
  inline String TargetName() const;

  // Kinematics Functions:

  inline double W() const {
    /// Calculates the center-of-mass energy 
    return this->TotalP4().M();
  }

  inline Double_t S() const {
    /// Calculates the Mandlestam variable @a s (\f$ W^2 \f$) 
    return this->TotalP4().M2();
  }

  inline TLorentzVector TotalP4() const {
    /// Calculates the total 4-momentum \f$ (p_{targ} + p_{phot} )\f$     
    return (_p4target + _photon.P4());
  }

  inline TLorentzVector P4Total() const {
    /// Calculates the total 4-momentum (same as TotalP4())     
    return this->TotalP4();
  }
  
  // gets 4-momentum of p1,p2 system
  TLorentzVector GetP4(const String &__p1,const String &__p2,float __mass = -1.0) const;

  inline TLorentzVector MissingP4() const {
    /// Returns the total missing 4-momentum
    TLorentzVector p4miss = this->TotalP4();
    for(int i = 0; i < _particles.Size(); i++) p4miss -= _particles[i].P4();
    return p4miss;
  }

  inline TLorentzVector P4Missing() const {
    /// Calculates the total missing 4-momentum (same as MissingP4())     
    return this->MissingP4();
  }

  TLorentzVector MissingP4Off(const String &__p,int __n = 0) const{    
    /// Returns the missing 4-momentum off the <em>(p,n)</em> particle.
    /** See GetParticle for particle indexing info */
    return (this->P4Total() - this->GetParticle(__p,__n).P4());
  }

  TLorentzVector MissingP4Off(const String &__p1,const String &__p2) const{
    /// Returns the missing 4-momentum off @a p1 and @a p2 particles.
    return (this->P4Total() - this->GetP4(__p1,__p2));
  }

  inline double MissingMass() const {
    /// Returns the total missing mass. 
    return this->MissingP4().M();
  }

  inline double MissingMass2() const {
    /// Returns the total missing mass squared. 
    return this->MissingP4().M2();
  }

  inline double MissingMassOff(const String &__p,int __n = 0) const {
    /// Returns the missing mass off the <em>(p,n)</em> particle.
    /** This function simply does MissingP4Off(__p,__n).M(). */
    return this->MissingP4Off(__p,__n).M();
  }

  inline double InvariantMass(const String &__p1,const String &__p2,
			      float __mass = -1.) const {
    /// Returns the invariant mass of @a p1 and @a p2.
    /** This function simply does GetP4(__p1,__p2,__mass).M().*/
    return this->GetP4(__p1,__p2,__mass).M();
  }
    
  double InvariantMass2(const String &__p1,const String &__p2,
			float __mass= -1.) const {
    /// Returns the invariant mass squared of @a p1 and @a p2.
    /** This function simply does GetP4(__p1,__p2,__mass).M2().*/
    return this->GetP4(__p1,__p2,__mass).M2();
  }

  // Some short-hand versions of other kinematic funcitons:

  inline double MM() const {
    /// Calculates the total missing mass (same as MissingMass())     
    return this->MissingMass();
  }

  inline double MM2() const {
    /// Calculates total missing mass squared (same as MissingMass2())     
    return this->MissingMass2();
  }

  inline double IM(const String &__p1,const String &__p2,float __mass = -1.) const {
    /// Same as InvariantMass() 
    return this->InvariantMass(__p1,__p2,__mass);
  }

  inline double IM2(const String &__p1,const String &__p2,float __mass = -1.) const {
    /// Same as InvariantMass2() 
    return this->InvariantMass2(__p1,__p2,__mass);
  }

  // Operators:

  const PArrayElement<Particle>& operator[](int __i) const {
    /// Returns a constant reference to the @a i'th PArrayElement object.
    return _particles[__i];
  }

  const PArrayElement<Particle>& operator()(int __i) const {
    /// Returns a constant reference to the @a i'th PArrayElement object.
    return _particles[__i];
  }
  
  //  ClassDef(Event,1); // ROOT class definition macro
};
//_____________________________________________________________________________

// inline member functions for Event class

inline String Event::TargetName() const {
  /// Returns the name of the target ("lh2" or "ld2").
  if(TMath::Abs(_p4target.M() - 0.93827) < 0.01) return String("lh2");
  if(TMath::Abs(_p4target.M() - 1.8756) < 0.01) return String("ld2");
  return String("?");
}
//_____________________________________________________________________________

// overload of << for Event
std::ostream& operator<<(std::ostream &__os,const Event &__ev);

//_____________________________________________________________________________

#endif /* _Event_H */
