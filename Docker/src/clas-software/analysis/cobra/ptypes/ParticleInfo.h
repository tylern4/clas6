// ParticleInfo class header file. -*- C++ -*- 
#ifndef _ParticleInfo_H
#define _ParticleInfo_H
/** @file ParticleInfo.h 
 *  @brief ParticleInfo class definition file.
 */
//_____________________________________________________________________________
// Includes:
// System Headers:
#include <iostream> // for cout
#include <string> 
#include <vector>
// ROOT Headers:
#include "TObject.h" // so we can make this class ROOT compatible
//_____________________________________________________________________________

// Global Types:
typedef std::string String;

//_____________________________________________________________________________

class ParticleInfo {

private:
  // Data Members (private):

  String _name; ///< Particle name
  String _texName; ///< Particle's LaTex name
  int _charge; ///< Electric charge in units of e
  double _mass; ///< Mass in GeV
  String _jp; ///< Spin-Parity 
  double _width; ///< Breit-Wigner width
  int _pdg_id; ///< PDG id scheme number
  int _geant_id; ///< GEANT id scheme number
  bool _isLepton; ///< Is this a lepton?
  bool _isMeson; ///< Is this a meson?
  bool _isBaryon; ///< Is this a baryon?

  // Functions (private):

  // Set all ParticleInfo data members
  void _SetInfo(const String &__name,int __q,double __m,const String &__jp,
		double __width,int __pdgId,int __geantId,const String &__type,
		const String &__texName);

  // Set to default values
  void _SetToDefaults();
  
  // Copy another ParticleInfo object's data members
  void _Copy(const ParticleInfo &__pinfo);

  // Friends:
  friend class ParticleTypes;

protected:

  // Create/Copy (protected):
  ParticleInfo(){this->_SetToDefaults();}
  ParticleInfo(const String &__name,int __q,double __m,const String &__jp,
	       double __width,int __pdgId,int __geantId,const String &__type,
	       const String &__texName);
  ParticleInfo(const String &__qName);

public:
  // Copy (public):
  ParticleInfo(const ParticleInfo &__pinfo);
  ParticleInfo& operator=(const ParticleInfo &__pinfo);

  // Destroy:
  virtual ~ParticleInfo();

  // Functions (public):

  inline const String& Name() const {
    /// Returns the particle's name (ex. @a pi-) 
    return _name;
  }
  
  inline const String& LaTexName() const {
    /// Returns the particle's LaTex name (ex. @a \pi^{-})
    return _texName;
  }

  inline int Charge() const {
    /// Returns the particle's charge in units of @a e   
    return _charge;
  }

  inline int Q() const {
    /// Returns the particle's charge in units of @a e   
    return _charge;
  }

  inline double Mass() const {
    /// Returns the particle's mass in \f$ GeV/c^2 \f$ 
    return _mass;
  }

  inline double M() const {
    /// Returns the particle's mass in \f$ GeV/c^2 \f$ 
    return _mass;
  }

  inline const String& JP() const {
    /// Returns the spin-parity (ex. @a 1/2+ for @a p )
    return _jp;
  }

  inline double Width() const {
    /// Returns the width in \f$ GeV \f$ 
    return _width;
  }

  inline int PDG_Id() const {
    /// Returns the @a pdg particle id number (ex. @a -211 for @a pi- )
    return _pdg_id;
  }

  inline int GEANT_Id() const {
    /// Returns the @a geant particle id number (ex. @a 14 for @a p )    
    return _geant_id;
  }

  inline bool IsLepton() const {
    /// Returns @a true if this is a lepton, @a false otherwise    
    return _isLepton;
  }

  inline bool IsMeson() const {
    /// Returns @a true if this is a meson, @a false otherwise     
    return _isMeson;
  }

  inline bool IsBaryon() const {
    /// Returns @a true if this is a baryon, @a false otherwise    
    return _isBaryon;
  }

  // returns the type of particle (meson,lepton,baryon)
  inline String Type() const;

  // prints ParticleInfo info to os
  void Print(std::ostream &__os = std::cout) const;

  //ClassDef(ParticleInfo,0); // ROOT class definition macro
};
//_____________________________________________________________________________

inline String ParticleInfo::Type() const {
  /// Returns the particle type (@a lepton,@a meson or @baryon).
  if(_isLepton) return "lepton";
  if(_isMeson) return "meson";
  if(_isBaryon) return "baryon";
  return String();
}

#endif /* _ParticleInfo_H */
