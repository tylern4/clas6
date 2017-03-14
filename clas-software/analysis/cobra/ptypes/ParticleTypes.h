// ParticleTypes class header file. -*- C++ -*-
#ifndef _ParticleTypes_H
#define _ParticleTypes_H
/** @file ptypes/ParticleTypes.h
 *  @brief ParticleTypes class definition file.
 */
//_____________________________________________________________________________
// Includes:
// System Headers:
#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
// ROOT Headers:
#include "TObject.h"
// Local Headers:
#include "ParticleInfo.h"
//_____________________________________________________________________________

// Global Types:
typedef std::string String;

//_____________________________________________________________________________

class ParticleTypes {

private:
  // Data Members (private):
  std::vector<ParticleInfo> _particles; ///< Vector of valid particles
  static ParticleTypes *_instance; ///< Pointer to instance of ParticleTypes

protected:
  // Create/Copy (protected):
  ParticleTypes();
  // the copy constructor/=operator are dummies...but they must be defined so
  // the compiler doesn't do it automatically and make them public
  ParticleTypes(const ParticleTypes &__ptypes){
    /// Dummy copy @a ctor...this is a @a Singelton class.
  };
  ParticleTypes& operator=(const ParticleTypes &__ptypes){
    /// Dummy @a = operator...this is a @a Singelton class.
    return *this;
  }

 public:
  // Destroy:
  virtual ~ParticleTypes();

  // Functions:

  inline static ParticleTypes* Instance(){
    /// Returns a pointer to the ParticleTypes instance.
    /**
     * If an instance doesn't exist, then it is created. This function is the
     * only way to obtain access to a ParticleTypes object.
     */
    if(!_instance) _instance = new ParticleTypes();
    return _instance;
  }

  // this function attempts to translate invalid names to valid ones
  bool ConvertName(const String &__name,String &__convName) const;

  // gets a constant reference to ParticleInfo object with name
  const ParticleInfo& GetParticle(const String &__name) const;

  inline const ParticleInfo& operator()(const String &__name) const {
    /// Same as GetParticle()
    return this->GetParticle(__name);
  }

  // array subscripting operator
  inline const ParticleInfo& operator[](int __i) const;

  inline const int Size() const {
    ///Returns the number of valid particles currently known.
    return (int)_particles.size();
  }

  // checks to see if name is a known particle name
  inline bool IsValidName(const String &__name) const;

  // prints a list of the known particle names to the screen
  void PrintValidNames() const;

  // prints all known particle info to the screen
  void Print(std::ostream &os = std::cout) const;

  //ClassDef(ParticleTypes,0); // ROOT class definition macro
};
//_____________________________________________________________________________

// inline member functions of ParticleTypes class:

inline const ParticleInfo& ParticleTypes::operator[](int __i) const {
  /// Subscripting operator.
  /**
   * @param i Index in _particles array
   *
   * <b> Asserts: </b>
   * @a i < Size()
   *
   */
  if(__i >= this->Size()){
    std::cout << "Error! <ParticleTypes::operator[]> index " << __i
          << " out of bounds. Number of known particles is "
          << this->Size() << "." << std::endl;
    abort();
  }
  return _particles[__i];
}

inline bool ParticleTypes::IsValidName(const String &__name) const {
  /// Returns @a true if @a name is a valid particle name, @a false otherwise.
  /**
   * This function only checks to see if @a name is exactly the same as the
   * name of a known particle. To see if @a name can be converted to a valid
   * name try running it thru ConvertName().
   */
  bool isValid = false;
  for(int i = 0; i < this->Size(); i++){
    if(_particles[i].Name() == __name){
      isValid = true;
      break;
    }
  }
  return isValid;
}
//_____________________________________________________________________________

#endif /* _ParticleTypes_H */
