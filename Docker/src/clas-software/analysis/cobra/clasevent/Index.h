// __Index class header file. -*- C++ -*-
// Author: Mike Williams
#ifndef _Index_H
#define _Index_H
/** @file Index.h
 * @brief Index class definition file.
 */
//_____________________________________________________________________________
// System Headers:
#include <vector>
#include <cmath>
// Local Headers:
#include "ParticleArray.h"
//_____________________________________________________________________________

typedef std::string String; 
class Photon;
class Particle;

// output message function for template interface functions
inline void PrintDummyTemplateMessage(const String &__funcName){
  std::cout << "Error! <" << __funcName << "> This is the default template "
	    << "version of this function, it's a dummy and should never be "
	    << "used. A specification of this function must be defined for "
	    << "each interface. If a specification is defined and you're "
	    << "getting this message, then the header file containing the "
	    << "specification isn't being included at the proper time and "
	    << "the compiler is generating this instantiation of the function."
	    << std::endl;
}
//_____________________________________________________________________________

class Index {

private:
  // Data Members:
  ParticleArray<int> _particles; ///< Charged particles to look for
  std::vector<int> _photIndex; ///< Tagged photon indicies
  int _numCharged; ///< Number of charged particles detected in the event
  std::vector<std::vector<int> > _indexCombos; ///< All event index combos
  int _currentIndexPointer; ///< Index to _indexCombos of current combo
  int _currentPhotIndexPointer; ///< Index to _photIndex of current photon
  String _id_scheme; ///< Particle id scheme (seb or part)
  float _dt; ///< Require |photon vertex time - event start time| < _dt

  // Functions (private):

  // copies ind
  void _Copy(const Index &__ind);

  // gets index combos from indicies in p_index
  bool _GetIndexCombos(const std::vector<std::vector<int> > &__p_index);

  // adds combo to index combos (if it should be added)
  void _AddIndexCombo(const std::vector<int> &__combo);

public:
  // Create/Copy/Destroy:  

  Index(){
    /// Default Constructor
    this->Reset();
  }

  Index(const Index &__ind){
    /// Copy Constructor
    this->_Copy(__ind);
  }

  virtual ~Index(){
    /// Destructor
    _particles.Clear();
    this->Reset();
  }

  inline Index& operator=(const Index &__ind){
    /// Assignment operator.
    this->_Copy(__ind);    
    return *this;
  }

  // Functions (public):

  // initialize the Index to look for p_names particles
  void Init(const std::vector<String> &__p_names);

  // reset for a new event
  void Reset();

  inline bool IsValid() const{
    /// Is this a valid combination of indicies for this event?
    if(_currentIndexPointer < (int)_indexCombos.size() 
       && _currentPhotIndexPointer < (int)_photIndex.size()) 
      return true;
    return false;
  }

  // Getters:

  inline int NumCharged() const {
    /// Returns the number of charged particles detected in the event.     
    return _numCharged;
  }

  inline int NumPhotons() const {
    /// Returns the number of @a good photons detected in the event.     
    return (int)_photIndex.size();
  }
  
  inline const String& IdScheme() const {
    /// Returns the particle id scheme being used (ex. @a seb)     
    return _id_scheme;
  }

  int NumEventCombos() const {
    /// Returns the number of event combos that can be made in this event.    
    return ((int)_indexCombos.size() * (int)_photIndex.size());
  }

  const ParticleArray<int>& GetParticles() const {
    /// Returns the @a _particles ParticleArray
    return _particles;
  }

  const std::vector<int>& GetIndicies() const{
    /// Returns the current set of indicies
    return _indexCombos[_currentIndexPointer];
  }

  inline int GetPhotonIndex() const {
    /// Returns the current photon index
    return _photIndex[_currentPhotIndexPointer];
  }

  // Setters:

  inline void SetIdScheme(const String &__scheme){
    /// Sets the particle id scheme     
    _id_scheme = __scheme;
  }

  inline void SetTimingCut(float __dt){
    /// Set the vertex timing cut variable
    _dt = __dt;
  }

  // Incrementing Operators:

  inline Index& operator++(){
    /// Prefix incrementer  
    _currentIndexPointer++;
    if(_currentIndexPointer >= (int)_indexCombos.size()){
      _currentIndexPointer = 0;
      _currentPhotIndexPointer++;
    }
    return *this;
  }

  inline Index& operator++(int){
    /// Postfix incrementer.
    return ++(*this);
  }

  // Template functions for interfaces:

  template<typename _InterTp> bool SetIndex(_InterTp __inter,
					    float __targOffset,float __vtime){
    /// Intializes the Index for a new event (_InterTp is the interface type)
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("Index::SetIndex");
    abort();
    return false; // for some reason some compilers want this here
  }

  template<typename _InterTp> void SetParticles(_InterTp __inter,
						ParticleArray<Particle> &__p,
						ParticleArray<Particle> &__ign,
						float __vtime){
    /// Sets p for the current set of indicies (for interface _InterTp)
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("Index::SetParticles");
    abort();
  }

  template<typename _InterTp> void SetPhoton(_InterTp __inter,Photon &__photon,
					     float __targOffset) const {
    /// Sets the Photon for the current photon index (for interface _InterTp)
    /** This is a dummy template, each interface must have its own
     *  specification of this function.
     */
    PrintDummyTemplateMessage("Index::SetPhoton");
    abort();
  }

};
//_____________________________________________________________________________

#endif /* _Index_H */
