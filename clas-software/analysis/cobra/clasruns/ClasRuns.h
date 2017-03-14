// ClasRuns class header file. -*- C++ -*-
#ifndef _ClasRuns_H
#define _ClasRuns_H
/** @file clasruns/ClasRuns.h
 * @brief ClasRuns class definition file.
 */
//_____________________________________________________________________________
// System Headers:
#include <iostream>
#include <string>
#include <vector>
// Local Headers:
#include "ClasRunPeriod.h"
//_____________________________________________________________________________

// Globals Types:

typedef std::string String; 

//_____________________________________________________________________________

class ClasRuns {

private:
  // Data Members (private):
  static ClasRuns *_instance; ///< Static pointer to instance of ClasRuns
  std::vector<ClasRunPeriod> _runs; ///< Vector of known CLAS run periods
  ClasRunPeriod _default_run; ///< Default run info

protected:
  // Create/Copy:

  // Default constructor
  ClasRuns();
  
  ClasRuns(const ClasRuns &__runs){
    /// Dummy copy ctor...this is a @a Singleton class     
  }

  ClasRuns& operator=(const ClasRuns &__runs){
    /// Dummy assignment operator...this is a @a Singleton class
    return *this;
  }

public:
  // Destory:

  virtual ~ClasRuns(){
    /// Destructor
    _runs.clear();
    _instance = 0; // without this it will never be recreated
  }

  // Functions:

  inline static ClasRuns* Instance(){
    /// Returns a pointer to the instance of ClasRuns
    if(!_instance) _instance = new ClasRuns();
    return _instance;
  }

  inline const ClasRunPeriod& GetRunPeriod(int __nrun) const {
    /// Returns a constant reference to the ClasRunPeriod for run @nrun    
    static int prevRun = -1;
    for(int i = 0; i < (int)_runs.size(); i++){
      if(_runs[i].IsValidRunNumber(__nrun)) return _runs[i];
    }
    // if we get here couldn't find a run for this run number...return default 
    if(prevRun != __nrun){
      std::cout << "Warning! <ClasRuns> " << __nrun << " is NOT a known run "
		<< "number...returning default values." << std::endl;
      prevRun = __nrun;
    }
    return _default_run;
  }

  inline const ClasRunPeriod& operator()(int __nrun) const {
    /// Same as GetRunPeriod(int).
    return this->GetRunPeriod(__nrun);
  }

  void Print(std::ostream &__os = std::cout) const {
    /// Print run info to @a os.
    __os << "ClasRuns:" << std::endl;
    for(int i = 0; i < (int)_runs.size(); i++) _runs[i].Print(__os);
  }

  //  ClassDef(ClasRuns,0); // ROOT definition macro
};

#endif /* _ClasRuns_H */
