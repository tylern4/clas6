// ClasRunPeriod class header file. -*- C++ -*-
/** @file ClasRunPeriod.h
 * @brief ClasRunPeriod class definition file.
 */
#ifndef _ClasRunPeriod_H
#define _ClasRunPeriod_H
// System Headers:
#include <iostream>
#include <string>
// ROOT Headers:
#include "TROOT.h"
//_____________________________________________________________________________

// Globals Types:

typedef std::string String; 

//_____________________________________________________________________________

/** @class ClasRunPeriod
 *  @brief Used by ClasRuns to store individual run period info.
 *
 *  This class is used by ClasRuns to store individual CLAS run period info.
 *  It stores the name (ex @a g1c), run number range, beam energy, torus
 *  current, target material/cell flags (following @a eloss convention) along
 *  with the target offset.
 *
 */

class ClasRunPeriod {

private:
  // Data Members (private):
  String _name; ///< CLAS run period name (ex. g1c)
  int _runNum_low; ///< 1st CLAS run number for this run period
  int _runNum_high; ///< Last CLAS run number for this run period
  float _e_beam; ///< Beam energy
  float _i_torus; ///< Torus current 
  int _targMat; ///< Target material flag (following eloss convention)
  int _targCell; ///< Target cell flag (following eloss convention)
  float _targOffset; ///< Target offset 

  // Functions (private):
  inline void _Copy(const ClasRunPeriod &__clasRun){
    /// Private function to copy ClasRunPeriod data members 
    _name = __clasRun._name;
    _runNum_low = __clasRun._runNum_low;
    _runNum_high = __clasRun._runNum_high;
    _e_beam = __clasRun._e_beam;
    _i_torus = __clasRun._i_torus;
    _targMat = __clasRun._targMat;
    _targCell = __clasRun._targCell;
    _targOffset = __clasRun._targOffset;
  }

  inline void _SetToDefault(){
    /// Private function to set data members to default values
    _name = String();
    _runNum_low = 0;
    _runNum_high = 0;
    _e_beam = 4.;
    _i_torus = 3375.;
    _targMat = 1; // lh2
    _targCell = 3; // use the g1c cell as default
    _targOffset = 0.;
  }
 
public:
  // Create/Copy/Destroy

  ClasRunPeriod(){
    /// Default Constructor
    this->_SetToDefault();
  }

  ClasRunPeriod(const ClasRunPeriod &__clasRun){
    /// Copy Constructor
    this->_Copy(__clasRun);
  }
    
  virtual ~ClasRunPeriod(){
    /// Destructor
  }

  ClasRunPeriod& operator=(const ClasRunPeriod &__clasRun){
    /// Assignment operator
    this->_Copy(__clasRun);
    return *this;
  }

  // Setters:

  inline void SetRunPeriod(const String &__name,int __nrunLow,int __nrunHigh,
			   float __eBeam,float __iTorus,int __targMat,
			   int __targCell,float __targOffset){ 
    /// Set @a ALL ClasRunPeriod data members.
    _name = __name;
    _runNum_low = __nrunLow;
    _runNum_high = __nrunHigh;
    _e_beam = __eBeam;
    _i_torus = __iTorus;
    _targMat = __targMat;
    _targCell = __targCell;
    _targOffset = __targOffset;
  }

  // Getters:

  const String& Name() const {
    /// Returns the name of the run period (ex. @a g11a)     
    return _name;
  }

  inline float BeamEnergy() const { 
    /// Returns the beam energy     
    return _e_beam;
  }
	
  inline float TorusCurrent() const {
    /// Returns the torus current     
    return _i_torus;
  }

  inline int TargetMaterialFlag() const {
    /// Returns the target material flag (@a eloss convention)      
    return _targMat;
  }

  inline int TargetCellFlag() const {
    /// Returns the target cell flag (@a eloss convention)      
    return _targCell;
  }

  inline float TargetOffset() const {
    /// Returns the target offset     
    return _targOffset;
  }

  // Functions:

  inline bool IsValidRunNumber(int __nrun) const {
    /// Returns true if \f$run_{low} \leq nrun \leq run_{high} \f$     
    return (__nrun >= _runNum_low && __nrun <= _runNum_high);
  }

  inline double TargetMass() const {
    /// Returns the target mass (uses the target material flag) 
    if(_targMat == 1) return 0.93827;
    if(_targMat == 2) return 1.87561;
    return 0.;
  }

  void Print(std::ostream &__os = std::cout) const {
    /// Print run info to @a os.
    __os << "[RunPeriod: " << _name << "(" << _runNum_low << "-" 
	 << _runNum_high << ") E0: " << _e_beam << std::endl;
    __os << "\tI: " << _i_torus << " Target: Material: " << _targMat
	 << " Cell: " << _targCell << " Offset: " << _targOffset << "]"
	 << std::endl;
  }

  //  ClassDef(ClasRunPeriod,0); // ROOT class defintion macro
};

#endif /* _ClasRunPeriod */
