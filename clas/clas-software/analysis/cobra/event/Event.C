// Author: Mike Williams (3/20/2005)
#include "Event.h"
/** @file Event.C
 * @brief Event class source file.
 */
//_____________________________________________________________________________

/** \class Event
 * \brief Fully ROOT compatible class for CLAS data analysis.
 *
 * Event is a fully ROOT compatible class. Thus, it can be written to a ROOT
 * file independently or as an entry in a TTree. It stores stores detected
 * charged particle info using a ParticleArray of Particle objects, tagged
 * photon info using a Photon object, the target 4-momentum, the interaction
 * vertex (most likely from @a MVRT), the CLAS run number and event number in 
 * that run, the event start time and the \f$ \chi^2 \f$ and number of degress
 * of freedom from kinematic fitting. For a complete list of charged particle
 * info stored see the Particle class, for a complete list of tagged photon
 * info stored see the Photon class.
 *
 * Event is a dynamic class, particles can be added or removed at any time 
 * using the AddParticle and RemoveParticle functions. However, for most 
 * applications the number of particles is constant and known at the time the
 * Event object is created. For these, a @a ctor is provided to create the 
 * object using a string to declare the particle names:
 *
 * <b> Example </b>:
 *
 * \include Event_Ctor.ex
 *
 * Only particles with @a valid particle names can be added to the event. This
 * is because these names are actually used as indicies to the @a singleton
 * class ParticleTypes which stores global particle info. See 
 * ListOfParticles.lst for a complete list of valid particle names and 
 * for more details on name conversions and what info is stored see 
 * ParticleTypes.
 *
 * To set the Event object for a given event, a variety of setter functions are
 * provided these all start with Set. This package is designed so that this
 * will actually be done by the ClasEvent class.
 *
 * To obtain info from an Event object that's allready been set there are a 
 * number of getter functions:
 *
 * <b>Example Usage: </b>
 *
 * \include Event_Getters.ex
 *
 * There are also a large number of functions for getting kinematic quantities,
 * such as 4-momenta of various particle combos or missing/invariant masses:
 *
 * \include Event_Kinematics.ex
 *
 * For events with identical final state particles, see GetParticle() 
 * documentation for info on how the subscripting works.
 *
 */
//_____________________________________________________________________________

//ClassImp(Event); // ROOT implementation macro

//_____________________________________________________________________________

void Event::_Copy(const Event &__ev){
  /// Copies all data members of @a ev to this Event object.
  /** Note: Only copies Event's data members, @a NOT TObject's. */
  _particles = __ev._particles;
  _photon = __ev._photon;
  _p4target = __ev._p4target;
  _vertex = __ev._vertex;
  _runNumber = __ev._runNumber;
  _eventNumber = __ev._eventNumber;
  _vertexTime = __ev._vertexTime;
  _chi2 = __ev._chi2;
  _ndf = __ev._ndf;
}
//_____________________________________________________________________________

Event& Event::operator=(const Event &__ev) {
  /// Assignment operator.
  this->TObject::operator=(__ev);
  this->_Copy(__ev);

  return *this;
}
//_____________________________________________________________________________

void Event::Zero() {
  /// Zero all the data members of this Event object.
  for(int i = 0; i < _particles.Size(); i++) _particles[i].Zero();
  _photon.Zero();
  _p4target.SetPxPyPzE(0.,0.,0.,0.);
  _vertex.SetXYZ(0.,0.,0.);
  _runNumber = 0;
  _eventNumber = 0;
  _chi2 = 0.;
  _ndf = 0;
  _vertexTime = 0.;
}
//_____________________________________________________________________________

TLorentzVector Event::GetP4(const String &__p1,const String &__p2,float __mass) const {
  /// Returns the 4-momentum of the @a p1 and @a p2 system.
  /**    
   * Note: If @a p1 or @a p2 is @a "missing", then the missing 4-momentum is
   * used. If @a mass if specified, it is taken as the mass of the missing 
   * particle.
   *
   * <b> Example </b> 
   *
   * <!-- 
   * For a p pi+ pi- final state, event.GetP4("pi+","pi-") returns the 
   * 4-momentum of the pi+ pi- system.
   *
   * If there was also a missing pi0, event.GetP4("pi+","missing") returns the
   * pi+ pi0 4-momentum using the missing 4-momentum for the pi0, while
   * event.GetP4("pi+","missing",0.13498) uses the missing 3-momentum
   * along with E = sqrt(p^2 + 0.13498^2) for the energy of the pi0.
   *
   * -->
   * \include Event_GetP4.ex
   *
   */
  TLorentzVector p4(0.,0.,0.,0.),p4temp;
  String miss,non_miss;
  if(__p1 == "missing" || __p2 == "missing"){
    // one of the particles is the missing particle
    if(__p1 == "missing"){
      miss = __p1;
      non_miss = __p2;
    }
    else{
      miss = __p2;
      non_miss = __p1;
    }
    p4 += this->GetParticle(non_miss).P4();
    if(__mass >= 0.){
      p4temp = this->MissingP4();
      p4temp.SetE(TMath::Sqrt(p4temp.P()*p4temp.P() + __mass*__mass));
      p4 += p4temp;
    }
    else p4 += this->MissingP4();
  }
  else{
    p4 = this->GetParticle(__p1).P4();
    if(__p1 == __p2) p4 += this->GetParticle(__p2,1).P4();
    else p4 += this->GetParticle(__p2).P4();
  }
  return p4;
}
//_____________________________________________________________________________

bool Event::TimingCut(float __t) const {
  /// Executes a vertex timing cut.
  /**
   * If all of the detected charged particle start counter vertex times are
   * within @a t ns of the photon's vertex time, the function returns @a true. 
   * Otherwise, it returns @a false.
   *
   */
  Float_t phoVtime = _photon.VertexTime();
  Bool_t isGood = true;
  for(int i = 0; i < _particles.Size(); i++){
    if(TMath::Abs(phoVtime - _particles[i].VertexTime()) > __t){
      isGood = false;
      break;
    }
  }
  return isGood;
}
//_____________________________________________________________________________

void Event::Print(std::ostream &__os) const {
  /// Dump contents of this Event object to std::ostream @a os.
  /** 
   * The std::ostream object, @a os, defaults to std::cout. Thus, 
   * Event::Print() prints the info to the screen.
   *
   */
  __os << "*******************************************************************"
       << "*************" << std::endl;;
  __os << "Event Object => gamma " << this->TargetName() << " -> " ;
  int i;
  for(i = 0; i < _particles.Size(); i++) __os << _particles[i].Name() << " "; 
  __os << "(" << this->MM() <<  ")" << std::endl;

  __os << "Run: " << this->RunNumber() << " Event: " << this->EventNumber() 
     << " Vertex Time: " << this->VertexTime() <<  std::endl;

  __os << _photon << std::endl;

  for(i = 0; i < _particles.Size(); i++){
    __os << _particles[i].Name() << " ";
    _particles[i].Particle::Print(__os);
  }
  __os << "Kinematic Fit: Ndf: " << this->Ndf() << " Chi2: " << this->Chi2() 
     << " Confidence Level: " << this->Prob() << std::endl;
  __os << "*******************************************************************"
       << "*************" << std::endl;;
  __os << std::endl;;
}
//_____________________________________________________________________________

/// Overload of std::ostream::operator<< for Event.
std::ostream& operator<<(std::ostream &__os,const Event &__ev){
  __ev.Print(__os);
  return __os;
}

