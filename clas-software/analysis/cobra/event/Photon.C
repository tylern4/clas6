//Author: Mike Williams 2/4/2004
#include "Photon.h"
//_____________________________________________________________________________

/** @file Photon.C
 * @brief Photon class source file.
 */
//_____________________________________________________________________________

/** \class Photon
 * \brief Utility class used by Event to store tagged photon info.
 *
 * This class is a utility class used by Event to store tagged photon 
 * information. It stores the photon's 4-momentum, E and T counter paddle id's
 * (from the tagger), vertex time (propagated to the vertex) and the pull
 *  quantity from kinematic fitting.
 *
 */ 
//_____________________________________________________________________________

//ClassImp(Photon); // ROOT class implementation macro

//_____________________________________________________________________________

void Photon::_Copy(const Photon &__ph){
  /// Copy @a _ph's data members (just the Photon data members).
  _p4 = __ph._p4;
  _vertexTime = __ph._vertexTime;
  _e_id = __ph._e_id;
  _t_id = __ph._t_id;
  _pull = __ph._pull;
}
//_____________________________________________________________________________

Photon& Photon::operator=(const Photon &__ph){
  /// Assignment Operator.
  this->TObject::operator=(__ph);
  this->_Copy(__ph);

  return *this;
}
//_____________________________________________________________________________

void Photon::Zero(){
  /// Sets All Photon data members to zero.
  _p4.SetPxPyPzE(0.,0.,0.,0.);
  _e_id = 0;
  _t_id = 0;
  _vertexTime = 0.0;
  _pull = 0.;
}
//_____________________________________________________________________________

void Photon::Print(std::ostream &__os) const {
  /// Print contents of this Photon object to @a os.
  /** 
   * The std::ostream object, os, defaults to std::cout. Thus, Photon::Print()
   * prints the info to the screen.
   *
   */
  __os << "[Photon: E: " << this->E() << " Vertex Time: "<< this->_vertexTime 
     << " E Id: " << this->_e_id << " T Id: " << this->_t_id 
     << " Pull: " << _pull << "]";
}
//_____________________________________________________________________________

/// Overload of std::ostream::operator<< for Photon.
std::ostream& operator<<(std::ostream &__os,const Photon &__ph){
  __ph.Print(__os);
  return __os;
}
//_____________________________________________________________________________
