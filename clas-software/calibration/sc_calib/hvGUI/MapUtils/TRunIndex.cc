
#include "TRunIndex.h"

ClassImp(TRunIndex)


TRunIndex::TRunIndex(){
// default constructor
//
}

TRunIndex::~TRunIndex(){
// default destructure
//
}

TRunIndex::TRunIndex(Int_t fpRunIndexId,Int_t    fpMinRun,
		     Int_t    fpMaxRun,  
		     Int_t    fpItemId,
                     Int_t    fpItemValueId,
		     const char *fpOfficer,
		     const char *fpTime,
		     const char *fpComment){
// Constructor Sets Variables 
//

  fRunIndexId  = fpRunIndexId;
  fMinRun      = fpMinRun;
  fMaxRun      = fpMaxRun;
  fItemId      = fpItemId;
  fItemValueId = fpItemValueId;
  fOfficer     = fpOfficer; 
  fTime        = fpTime;
  fComment     = fpComment;

}

void TRunIndex::Set(Int_t fpRunIndexId,Int_t    fpMinRun,
		    Int_t    fpMaxRun,  
		    Int_t    fpItemId,
		    Int_t    fpItemValueId,
		    const char *fpOfficer,
		    const char *fpTime,
		    const char *fpComment){
  // Sets Variables 
  //

  fRunIndexId  = fpRunIndexId;
  fMinRun      = fpMinRun;
  fMaxRun      = fpMaxRun;
  fItemId      = fpItemId;
  fItemValueId = fpItemValueId;
  fOfficer     = fpOfficer; 
  fTime        = fpTime;
  fComment     = fpComment;

}

