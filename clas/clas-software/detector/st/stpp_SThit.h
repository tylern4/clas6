#ifndef __ST_HIT_H__
#define __ST_HIT_H__

#include "stpp_STdata.h"
#include "stpp_STposition.h"

/// the ST hit is a match between an entry in the ST banks 
/// and the intersection of a track with the ST plane
class SThit: public STdata, public STposition {
public:
  /// parameters for constructor: calibrated time and 
  /// intersection between track and ST plane
  SThit(const STdata& t, const STposition& p);

  /// time from STdata corrected by light propagation
  double GetHitTime();              

  /// copy information to STR bos bank
  void FetchToBos(str_t*  strptr);  

  /// copy information to STRE bos bank
  void FetchToBos(stre_t* streptr); 

  /// energy deposit in ST, should be corrected for track angle
  double GetEnergyDeposit();        
};

#endif
