//$Id: DOCA.cc,v 1.2 2008/03/27 14:22:50 pasyuk Exp $

#include "DOCA.h"
//------------------ DOCA stuff -----------------------------------------
DOCA PartTrack::GetDOCA(const PartTrack trk) {
  double mm = m * trk.m;
  double nominator = mm * mm - 1.; /// scalar product of directions
  double x0=0;
  double x1=0;

  if (fabs (nominator) < 1.E-15) { /// parallel, x0 = 0
    x1 = (b - trk.b) * m / mm;
  } 
  else {
    double A = (b - trk.b) * m;
    double B = (b - trk.b) * trk.m;
    x0 = (A-mm*B) / nominator;
    x1 = (mm*A-B) / nominator;
  }
  return DOCA(TVector3(b+x0*m), TVector3(trk.b+x1*trk.m));
}

ostream& operator << (ostream& os, const DOCA& v) {
  os << " x0=" << v.p0.X() << " y0=" << v.p0.Y() << " z0=" << v.p0.Z() 
     << "     x1=" << v.p0.X()<< " y1=" << v.p0.Y()<< " z1=" << v.p0.Z();
  return os;
}

double* DOCA::Points4Demo() {
  double* retval = new double[6];
  p0.GetXYZ(retval);
  p1.GetXYZ(retval+3);
  return retval;
}


double* PartTrack::Points4Demo() {
  double* retval = new double[6];
  TVector3 q, r;
  for (int i=1; i<20; i++) {
    q = b + i * m;
    if (fabs (q.X()) > 10 || fabs (q.Y()) > 10 || fabs (q.Z()) > 10) break;
  }

  for (int i=-1; i>-20; i--) {
    r = b + i * m;
    if (fabs (r.X()) > 10 || fabs (r.Y()) > 10 || fabs (r.Z()) > 10) break;
  }

  q.GetXYZ(retval);
  r.GetXYZ(retval+3);
  return retval;
}
//------------------ end DOCA stuff -------------------------------------
