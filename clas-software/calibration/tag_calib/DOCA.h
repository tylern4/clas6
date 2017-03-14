#ifndef __DOCA_H
#define __DOCA_H
using namespace std;

#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <TLorentzVector.h>

//------------------ DOCA stuff -----------------------------------------
class DOCA;

class PartTrack {
  TVector3 b;
  TVector3 m;
protected:
  void CopyTrack(const PartTrack& pt) {
    b = pt.b;
    m = pt.m;
  }
public:
  PartTrack ( const TVector3 b_, const TVector3 m_ ) : b(b_), m(m_.Unit()) { }
  double* Points4Demo();
  double VertZ() {return b.Z(); }      // z-component of particle vertex
  DOCA GetDOCA(const PartTrack trk);   // gives the DOCA
};

class DOCA {
  TVector3 p0;  // internally used:  intersection between DOCA line and trk1
  TVector3 p1;  //                   intersection between DOCA line and trk2
public:
  DOCA(const TVector3 p0_, const TVector3 p1_ ): p0(p0_), p1(p1_) {}
  double Distance() { return (p0-p1).Mag(); }  // distance of closet approach
  double Z() { return (p0+p1).Z() / 2.; }      // z-component of inters. point
  TVector3 Point() { return (p0+p1) * 0.5; }   // intersection point
  double* Points4Demo(); 
  friend ostream& operator << (ostream& os, const DOCA& v);  // dump on screen
};
//------------------ end DOCA stuff -------------------------------------


#endif
