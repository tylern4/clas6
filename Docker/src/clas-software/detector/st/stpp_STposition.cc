#include "stpp_STposition.h"

extern "C" {
#include "bostypes.h"
}
#include "stpp_JBosBank.h"
#include "stpp_JCalibration.h"


extern JCalibration* gCalib;

//-------------------------------------------------------
/// JVector3 stuff
JVector3::JVector3 (double x_, double y_, double z_) :
  x(x_), y(y_), z(z_) {};

JVector3::JVector3 (const vector3_t& v) :
  x(v.x), y(v.y), z(v.z) {}

JVector3& JVector3::Unit() {
  double r = Mag();
  x /= r;
  y /= r;
  z /= r;
  return *this;
}

double JVector3::ViehDeg() const {
  return atan2(y,x) * DEG;
}

void JVector3::RotateZ(double angle) {
  double s = sin(angle);
  double c = cos(angle);
  double xx = x;
  x = c*xx - s*y;
  y = s*xx + c*y;
}

void JVector3::SetMagThetaPhi(double mag, double theta, double phi) {
     double amag = fabs(mag);
     x = amag * sin(theta) * cos(phi);
     y = amag * sin(theta) * sin(phi);
     z = amag * cos(theta);
}

const JVector3& JVector3::operator+=(const JVector3& v) {
  x += v.x;
  y += v.y;
  z += v.z;
  return *this;
}

double operator*(const JVector3& a, const JVector3& b) {
  return a.x*b.x + a.y*b.y + a.z*b.z;
}

JVector3 operator*(const double s, const JVector3& v) { 
  return JVector3(s*v.x, s*v.y, s*v.z);
}

JVector3 operator+(const JVector3& a, const JVector3& b) {
  return JVector3(a.x+b.x, a.y+b.y, a.z+b.z);
}

JVector3 operator-(const JVector3& a, const JVector3& b) {
  return JVector3(a.x-b.x, a.y-b.y, a.z-b.z);
}

/// end JVector3

//-------------------------------------------------------

/// STplane stuff
STplane::STplane(int id_, int sector_): id(id_), sector(sector_) {
  switch (id) {
  case 1:
    norm = JVector3(1, 0, 0);
    d_origin = STposition::x0;
    break;
  case 2: 
    {
      double noseAngleRad = gCalib->GetFloat(geo_st_noseAngle, 0) / DEG;
      double cosNoseAng   = cos(noseAngleRad);
      double sinNoseAng   = sin(noseAngleRad);
      norm = JVector3(cosNoseAng, 0, sinNoseAng);
      d_origin = STposition::z1 * sinNoseAng
	+ STposition::x0 * cosNoseAng;
    }
    break;
  default:
    throw "STplane::STplane: plane id must be either 1 or 2";
  }
}

int STplane::Intersection(JVector3& pos, JVector3& dir, double& tlen) {
  double rotang =  (1. - sector) * 60. / DEG;

  /// rotate vertex and direction vector into sector system
  dir = dir.Unit();
  dir.RotateZ(rotang);
  pos.RotateZ(rotang);

  /// calculate distance from vertex to intersection
  double nominator = dir * norm;
  if (fabs(nominator) < 1.E-9) return -1;
  tlen = (d_origin - pos * norm) / nominator;

  /// positon on detector plane
  pos += tlen * dir;
  return 0;
}
//-------------------------------------------------------


STposition::STposition() : 
  use_tdpl(true), moretrks(false), errcode(0), 
  goodplane(0), isnose(false), geoscale(1.), angSTplane(0.) {};

STposition::STposition(int sector_, int ixdpl_, bool use_tdpl_) : 
  sector(sector_), ixdpl(ixdpl_), use_tdpl(use_tdpl_), moretrks(false),
  errcode(0), goodplane(0), isnose(false), geoscale(1.), angSTplane(0.) {
  ConstructorXDPL();
}

STposition::STposition(int idcpb) : 
  use_tdpl(true), moretrks(false), errcode(0), goodplane(0), 
  isnose(false), geoscale(1.), angSTplane(0.) {
  Jdcpb dcpb;
  if (!dcpb.Found() ) return;
  if (idcpb >= dcpb.GetNrows()) { errcode =-3; return; }

  sector     =  dcpb[idcpb].sctr / 100;
  int itbtr  =  dcpb[idcpb].sctr % 100 - 1;
  Jtbtr tbtr;
  if (!tbtr.Found()) { errcode =-4; return; }
  ixdpl = tbtr[itbtr].itr_sec % 100 - 1;

  ConstructorXDPL();
}

STposition::STposition(int sector_, JVector3 vert, JVector3 p) :
  sector(sector_), ixdpl(-1), use_tdpl(true), moretrks(false),
  errcode(0), goodplane(0), pos(vert), dir(p), tlen(0),
  isnose(false), geoscale(1.), angSTplane(0.) {
  
  STplane pl1(1, sector);
  if (pl1.Intersection(pos, dir, tlen) < 0) {
    errcode = -13; return;
  }
  goodplane = 1;

  if (pos.Z() > z1) {
    pos = vert; 
    dir = p;
    STplane pl2(2, sector);
    if (pl2.Intersection(pos, dir, tlen) < 0) {
      errcode = -14; return;
    }
    goodplane = 2;
  }

  ConstructorGeo();
}

double STposition::ViehDeg() const {     /// phi in degree in sector coordinates
  return  pos.ViehDeg() ;
}

int STposition::GetSegment8() const {
  int segm = (int) floor ( (30. + ViehDeg()) / 7.5);
  if (segm < 0) segm = 0;
  if (segm > 7) segm = 7;
  return segm;
}

void STposition::ConstructorXDPL() {
  Jxdpl xdpl(sector, use_tdpl);
  if (!xdpl.Found()) { errcode =-5; return; }
  if (ixdpl < 0 || ixdpl*10+9 >= xdpl.GetNrows()) { errcode =-6; return; }
  if (xdpl.GetNrows() > 10) moretrks = true;

  /// which one is closer
  if (xdpl.good(ixdpl,1)) goodplane |= 1; 
  if (xdpl.good(ixdpl,2)) goodplane |= 2;
  if (!goodplane) { errcode = -7; return; }
  int ipl  = xdpl.GetBestSTplane(ixdpl);

  pos = JVector3(xdpl.GetPosition(ixdpl, ipl));
  dir = JVector3(xdpl.GetDirection(ixdpl, ipl));
  tlen = xdpl.GetTrackLength(ixdpl, ipl);
  ConstructorGeo();
}

void STposition::ConstructorGeo() {
  /// part of backward direction which is not covered by ST
  if (pos.Z() < z0) { 
    stpos = 0.;
  }

  /// leg part
  else if (pos.Z() < z1) {
    stpos = pos.Z() - z0;

    /// bending towards nose
    if (pos.Z() > z1 - zbend) {
      angSTplane = tan ((pos.Z() - z1 + zbend) / rbend);
    }
  }

  /// nose part
  else {
    isnose = true;
    angSTplane = gCalib->GetFloat(geo_st_noseAngle, 0) / DEG;
    double nosepos = (pos.Z() - z1) / cos(angSTplane);
    stpos = nosepos + z1 - z0;

    /// bending towards leg
    if (nosepos < zbend) {
      angSTplane -= tan ((zbend - nosepos) / rbend);
    }
  }

  /// normal vector for ST plane in sector coordinate system
  JVector3 stPlaneNorm(cos(angSTplane), 0, sin(angSTplane));

  geoscale = dir * stPlaneNorm;
}

double STposition::x0 = 0;
double STposition::z0 = 0;
double STposition::z1 = 0;
double STposition::rbend = 5.08;
double STposition::zbend = 0.;

void stpp_init_geometry() {
  STposition::x0 =
    gCalib->GetFloat(geo_st_dist2bl, 0) -
    gCalib->GetFloat(geo_st_thickness, 0)/2.;

  STposition::z0 = 
    gCalib->GetFloat(geo_st_offset, 2) -
    gCalib->GetFloat(geo_st_legVolume, 2)/2.;

  STposition::z1 = 
    gCalib->GetFloat(geo_st_offset, 2) +
    gCalib->GetFloat(geo_st_legVolume, 2)/2.;

  STposition::rbend =
    gCalib->GetFloat(geo_st_bendRadius, 0);

  STposition::zbend = STposition::rbend 
    * tan(gCalib->GetFloat(geo_st_noseAngle, 0)/2./DEG);

  cout << "ST geometry initialized, leg z0=" 
       << STposition::z0 << ", leg z1=" << STposition::z1
       << ", bending radius=" <<  STposition::rbend
       << ", part of leg/nose affected by bending=" << STposition::zbend
       << endl;
}

