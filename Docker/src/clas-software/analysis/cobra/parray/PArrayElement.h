// PArrayElement class header file. -*- C++ -*- 
#ifndef _PArrayElement_H
#define _PArrayElement_H
/** @file PArrayElement.h
 *  @brief ParrayElement template class definition file.
 */
//_____________________________________________________________________________
// Includes:
#include <iostream> // for std output streams
#include <string> // for std::string class
#include "TypeInfo.h" // for template primatives/pointer recognition
#include "ParticleTypes.h" // for ParticleTypes class
#include "PArrayElement_Base.h" // for PArrayElement_Base class
#include "TObject.h"
//_____________________________________________________________________________

// Global Types:
typedef std::string String; 
//_____________________________________________________________________________
/** @class PArrayElement
 *  @brief Template class used to store each element in a ParticleArray.
 *
 *  ParticleArray uses this class to store its elements. This class stores a
 *  particle name that must be a valid name in the instance of ParticleTypes.
 *  Whenever the name is set, PArrayElement grabs a pointer to the 
 *  ParticleTypes instance to check that the name is valid (or can be converted
 *  to a valid name). See ParticleTypes for details on valid names. 
 *
 *  PArrayElement inherits from PArrayElement_BaseType whose type is determined
 *  by the TypeInfo template class. For primitives, this is PArrayElement_Base,
 *  a simple class that, in essence, makes it possible to inherit from a 
 *  primitive. For objects, PArrayElement simply inherits from the object 
 *  itself. Thus, if PArrayElement is instantiated with an object, it inherits
 *  all of that objects member functions (except any that share a name with a
 *  ParticleInfo member function...this should be rare) and can be converted to
 *  the objects type with the conversion operator. So, in essence, it can be 
 *  treated as having the same type as the object that instantiates it.
 *
 *  <b> Example Usage: </b> 
 *
 *  \include PArrayElement_Conversion.ex
 *
 *  PArrayElement provides wrappers to all of the ParticleInfo public member
 *  functions. Thus, PArrayElement can not only be treated as being an object
 *  of the type used to instantiate it, but also as a ParticleInfo object 
 *  depending on the situation it's used in.
 *
 *  Note: This class is fully ROOT compitable.
 */
//_____________________________________________________________________________

// PArrayElement template class (default template).
template<typename _Tp> 
class PArrayElement : public TypeInfo<_Tp>::PArrayElement_BaseType {

private:
  // Data Members (private):
  String _ptypes_name; ///< Particle name in ParticleTypes particle list

  // typdef's (private):
  typedef typename TypeInfo<_Tp>::PArrayElement_BaseType _BaseType;
  typedef typename TypeInfo<_Tp>::ParamType _ParamType;

public:
  // Create/Copy/Destory:

  PArrayElement():_BaseType(){
    /// Default Constructor
  }

  PArrayElement(const String &__name,typename TypeInfo<_Tp>::ParamType __value 
		= _Tp()):_BaseType(__value){
    /// Constructor
    /** @param name Name of particle in ParticleTypes class 
     *  @param value Value to be stored (defaults to @a _Tp())
     */
    // GetParticle will convert the name if necessary
    _ptypes_name = ParticleTypes::Instance()->GetParticle(__name).Name();
  }

  PArrayElement(const PArrayElement<_Tp> &__pelem):_BaseType(__pelem){
    /// Copy Constructor
    _ptypes_name = __pelem._ptypes_name;
  }

  PArrayElement& operator=(const PArrayElement<_Tp> &__pelem){
    /// Assignment operator
    this->_BaseType::operator=(__pelem);
    _ptypes_name = __pelem._ptypes_name;
  
    return *this;
  }

  virtual ~PArrayElement(){
    /// Destructor
  }

  // Assign:
  PArrayElement& operator=(_ParamType __value){
    /// Assignment operator
    this->_BaseType::operator=(__value);
    return *this;
  }

  operator _Tp(){
    /// Conversion operator     
    return (_Tp)(((_BaseType)(*this)));
  }
  
  inline bool operator==(const PArrayElement &__pelem) const {
    /// Comparison operator
    return (this->_BaseType::operator==(__pelem) 
	    && _ptypes_name == __pelem._ptypes_name);
  }

  // Functions:  

  void SetName(const String &__name){
    /// Set the particle name (must be a valid name in ParticleTypes)
    _ptypes_name = ParticleTypes::Instance()->GetParticle(__name).Name();
  }

  void Print() const {
    /// Print info to @a cout
    /** Not implemented to generic @a ostream due to interference of having a
     *  templated @a << operator with ROOT dictionary generation.
     */
    std::cout << "PArrayElement: [" << _ptypes_name << "] ";
    ((_BaseType)(*this)).Print();
  }

  // ParticleInfo function wrappers:

  inline const ParticleInfo& GetParticleInfo() const {
    /// Get a constant reference to the ParticleInfo object in ParticleTypes.
    /** Returns a @a const& to the particle in the instance of ParticleTypes
     *  whose name is the same as @a _ptypes_name 
     */ 
    return ParticleTypes::Instance()->GetParticle(_ptypes_name);
  }

  inline const String& Name() const {
    /// Particle's name (ex. @a pi- for \f$ \pi^- \f$)
    return _ptypes_name;
  }

  inline const String& LaTexName() const {
    /// Particle's LaTex name (ex. @a \pi^{-} for \f$ \pi^- \f$)
    return this->GetParticleInfo().LaTexName();
  }

  inline int Charge() const { 
    /// Particle's charge in units of e (ex @a -1 for @a e-).    
    return this->GetParticleInfo().Q();
  }
  
  inline int Q() const {
    /// Particle's charge in units of e (ex @a -1 for @a e-).    
    return this->GetParticleInfo().Q();
  }

  inline double Mass() const {
    /// Particle's mass in \f$ GeV/c^2 \f$ 
    return this->GetParticleInfo().M();
  }

  inline double M() const {
    /// Particle's mass in \f$ GeV/c^2 \f$ 
    return this->GetParticleInfo().M();
  }

  inline const String& JP() const {
    /// Particle's Spin-Parity (ex. @a 1/2+ for @a p)    
    return this->GetParticleInfo().JP();
  }

  inline double Width() const {
    /// Particle's Breit-Wigner width in @a GeV.
    return this->GetParticleInfo().Width();
  }

  inline int PDG_Id() const {
    /// Particle's @a PDG id number (ex. @a 321 for \f$ k^+ \f$).    
    return this->GetParticleInfo().PDG_Id();
  }

  inline int GEANT_Id() const {
    /// Particle's @a GEANT id number (ex. @a 14 for @a p).
    return this->GetParticleInfo().GEANT_Id();
  }

  inline bool IsLepton() const {
    /// Is this a lepton? 
    return this->GetParticleInfo().IsLepton();
  }

  inline bool IsMeson() const {
    /// Is this a meson? 
    return this->GetParticleInfo().IsMeson();
  }

  inline bool IsBaryon() const {
    /// Is this a baryon?     
    return this->GetParticleInfo().IsBaryon();
  }

  inline String Type() const {
    /// Particle's type (@a lepton,meson,baryon).    
    return this->GetParticleInfo().Type();
  }

  //ClassDef(PArrayElement,1); // ROOT class definition...ALL instantiations
};
//_____________________________________________________________________________

#endif /* _PArrayElement */
