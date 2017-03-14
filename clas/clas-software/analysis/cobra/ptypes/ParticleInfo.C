#include "ParticleInfo.h"
/** @file ParticleInfo.C
 *  @brief ParticleInfo class source file.
 */
//_____________________________________________________________________________

/** @class ParticleInfo
 *  @brief Used by ParticleTypes to store individual particle information.
 *
 * ParticleInfo is a class used by ParticleTypes to store information for each
 * @a valid particle known to it. ParticleInfo stores a particle's name, 
 * charge, mass, spin-parity, Breit-Wigner width, pdg id and geant id numbers,
 * along with whether it's a lepton, meson or baryon. It also stores the 
 * particle's name in a LaTex string.
 *
 * This class is only meant
 * as an array element for ParticleTypes. The only @a public @a non-const
 * methods are the @a dtor, the copy @a ctor and the @a = operator. Thus, 
 * outside of ParticleTypes (which is a @a friend class), only copies of 
 * other ParticleInfo objects made by ParticleTypes can be obtained.
 *
 */
//_____________________________________________________________________________

//ClassImp(ParticleInfo); // ROOT class implementation macro

//_____________________________________________________________________________

ParticleInfo::ParticleInfo(const String &__name,int __q,double __m,
			   const String &__jp,double __width,int __pdgId,
			   int __geantId,const String &__type,
			   const String &__texName){
  /// Constructor (protected)
  /** 
   * @param name Name (ex. @a pi-)
   * @param q Charge in units of @a e (ex. @a -1 for @a e-)
   * @param m Mass in \f$ GeV/c^2 \f$ 
   * @param jp Spin-parity (ex. @a 1/2+ for @a p)
   * @param width Breit-Wigner width in \f$ GeV \f$
   * @param pdgId @a pdg particle id number (ex. @a -211 for @a pi- )
   * @param geantId @a geant particle id number (ex. @a 14 for @a p )
   * @param type Type of particle (@a lepton,@a meson or @a baryon)
   * @param texName LaTex name of the particle (ex. @a \pi^{-})
   *
   * All parameters are passed directly to _SetInfo().
   *
   */
  this->_SetInfo(__name,__q,__m,__jp,__width,__pdgId,__geantId,__type,__texName);
}
//_____________________________________________________________________________

ParticleInfo::ParticleInfo(const String &__qName){
  /// Constructor (protected)
  /**
   * @param qName Charge name (@a +,@a - or @a 0)
   *
   * This constructor is used to create a generic charged particle. All data
   * members, other than the charge, are set to the default values.
   *
   */
  this->_SetToDefaults();
  _name = __qName;
  _texName = _name;
  if(_name == "+") _charge = +1;
  else if(_name == "0") _charge = 0;
  else if(_name == "-") _charge = -1;
}
//_____________________________________________________________________________

ParticleInfo::ParticleInfo(const ParticleInfo &__pinfo){
  /// Copy Constructor
  /** See _Copy() for details */
  this->_Copy(__pinfo);
}
//_____________________________________________________________________________

ParticleInfo::~ParticleInfo(){
  /// Destructor
}
//_____________________________________________________________________________

void ParticleInfo::_Copy(const ParticleInfo &__pinfo){
  /// Make @a this a copy of @a pinfo (private function).
  _name = __pinfo._name;
  _charge = __pinfo._charge;
  _mass = __pinfo._mass;
  _jp = __pinfo._jp;
  _width = __pinfo._width;
  _pdg_id = __pinfo._pdg_id;
  _geant_id = __pinfo._geant_id;
  _isLepton = __pinfo._isLepton;
  _isMeson = __pinfo._isMeson;
  _isBaryon = __pinfo._isBaryon;
  _texName = __pinfo._texName;
}
//_____________________________________________________________________________

ParticleInfo& ParticleInfo::operator=(const ParticleInfo &__pinfo){
  /// Assignment operator
  /** See _Copy() for details */
  this->_Copy(__pinfo);

  return *this;
}
//_____________________________________________________________________________

void ParticleInfo::_SetInfo(const String &__n,int __q,double __m,
			    const String &__j_p,double __wd,int __pdg,
			    int __geant,const String &__type,
			    const String &__texName){
  /// Set @a ALL ParticleInfo data members.
  /**
   * @param n Name (ex. @a pi-)
   * @param q Charge in units of @a e (ex. @a -1 for @a e-)
   * @param m Mass in \f$ GeV/c^2 \f$ 
   * @param j_p Spin-parity (ex. @a 1/2+ for @a p)
   * @param wd Breit-Wigner width in \f$ GeV \f$
   * @param pdg @a pdg particle id number (ex. @a -211 for @a pi- )
   * @param geant @a geant particle id number (ex. @a 14 for @a p )
   * @param type Type of particle (@a lepton,@a meson or @a baryon)
   * @param texName LaTex name of the particle (ex. @a \pi^{-})
   *
   */
  _name = __n;
  _charge = __q;
  _mass = __m;
  _jp = __j_p;
  _width = __wd;
  _pdg_id = __pdg;
  _geant_id = __geant;
  _texName = __texName;

  // find out which type it is
  _isLepton = false;
  _isMeson = false;
  _isBaryon = false;
  if(__type == "lepton") _isLepton = true;
  else if(__type == "meson") _isMeson = true;
  else if(__type == "baryon") _isBaryon = true;  
}
//_____________________________________________________________________________
    
void ParticleInfo::_SetToDefaults(){
  /// Set data members to default values (private function)
  /**
   * Default values are:
   *
   * <em> charge,mass,width,pdg_id,geant_id  </em> \f$ \rightarrow \f$ @a 0
   *
   * <em> isLepton,isMeson,isBaryon </em> \f$ \rightarrow \f$ @a false
   *
   * <em> name,jp </em> \f$ \rightarrow \f$ @a string()
   *
   */
  _name = String();
  _texName = String();
  _charge = 0;
  _mass = 0.;
  _jp = String();
  _width = 0.;
  _pdg_id = 0;
  _geant_id = 0;
  _isLepton = false;
  _isMeson = false;
  _isBaryon = false;
}
//_____________________________________________________________________________
    
void ParticleInfo::Print(std::ostream &__os) const {
  /// Print info to os.
  /** @param os Any ostream object (defaults to @a std::cout) */
  __os << _name << "(" << _jp << ") [" << this->Type() << "] mass: " << _mass 
       << " q:" << _charge << " width: " << _width << " Ids(pdg:" << _pdg_id 
       << ",geant:" << _geant_id << ") LaTex (" << _texName << ")" 
       << std::endl;
}
//_____________________________________________________________________________
 
